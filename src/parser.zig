const std = @import("std");
const Allocator = std.mem.Allocator;
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("lexer.zig").Token;
const TokenType = @import("lexer.zig").TokenType;

pub const Error = Allocator.Error || error{ParseError};

fn expectToken(lexer: *Lexer, tokenType: TokenType) !Token {
    if (@as(TokenType, lexer.curr) == tokenType) {
        var old = lexer.curr;
        _ = lexer.next();
        return old;
    }
    std.debug.print("{s}: Expected token of type \"{s}\" but got {s}\n", .{ lexer.loc, @tagName(tokenType), lexer.curr });
    return error.ParseError;
}

fn expectLineEnd(lexer: *Lexer) !void {
    if (lexer.isLineEnd()) {
        _ = lexer.next();
        return;
    }
    std.debug.print("{s}: Expected end of line but got {s}\n", .{ lexer.loc, lexer.curr });
    return error.ParseError;
}

pub const Declaration = struct {
    identifier: []const u8,
    value: Expression,

    fn parse(allocator: Allocator, lexer: *Lexer) !Declaration {
        _ = try expectToken(lexer, Token.KeywordVar);
        var nameToken = try expectToken(lexer, TokenType.Identifier);
        _ = try expectToken(lexer, Token.Equals);
        var initExpr = try Expression.parse(allocator, lexer);

        return Declaration{
            .identifier = nameToken.Identifier,
            .value = initExpr,
        };
    }

    fn deinit(self: *Declaration) void {
        self.value.deinit();
    }

    pub fn format(
        self: Declaration,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        return std.fmt.format(writer, "Declaration({s} = {s})", .{ self.identifier, self.value });
    }
};

const ArgumentList = struct {
    arguments: []Expression,
    allocator: Allocator,

    fn parse(allocator: Allocator, lexer: *Lexer) !ArgumentList {
        var arguments = std.ArrayList(Expression).init(allocator);
        defer arguments.deinit();
        errdefer for (arguments.items) |*argument| {
            argument.deinit();
        };

        while (lexer.curr != .ParenClose) {
            var expr = try Expression.parse(allocator, lexer);
            errdefer expr.deinit();

            try arguments.append(expr);
            if (lexer.curr != .Comma) {
                break;
            } else {
                _ = lexer.next();
            }
        }

        return ArgumentList{
            .arguments = arguments.toOwnedSlice(),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *ArgumentList) void {
        for (self.arguments) |*argument| {
            argument.deinit();
        }
        self.allocator.free(self.arguments);
        self.arguments = undefined;
    }

    pub fn format(
        self: ArgumentList,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        for (self.arguments) |argument| {
            try std.fmt.format(writer, "{s}, ", .{argument});
        }
    }
};

pub const Expression = union(enum) {
    Number: i32,
    Variable: []const u8,
    FunctionCall: struct {
        name: []const u8,
        argList: ArgumentList,
    },
    BinaryOp: struct {
        operand1: *Expression,
        operand2: *Expression,
        operator: TokenType,
        allocator: Allocator,
    },

    fn parseFactor(allocator: Allocator, lexer: *Lexer) !Expression {
        _ = allocator;
        switch (lexer.curr) {
            .Number => {
                defer _ = lexer.next();
                return Expression{ .Number = lexer.curr.Number };
            },
            .Identifier => {
                var nameToken = try expectToken(lexer, TokenType.Identifier);
                if (lexer.curr != Token.ParenOpen) {
                    return Expression{ .Variable = nameToken.Identifier };
                }
                _ = lexer.next();
                var argList = try ArgumentList.parse(allocator, lexer);
                errdefer argList.deinit();
                _ = try expectToken(lexer, TokenType.ParenClose);
                return Expression{ .FunctionCall = .{
                    .name = nameToken.Identifier,
                    .argList = argList,
                } };
            },
            .ParenOpen => {
                _ = lexer.next();
                var expr = try Expression.parse(allocator, lexer);
                errdefer expr.deinit();
                _ = try expectToken(lexer, TokenType.ParenClose);
                return expr;
            },
            else => {
                std.debug.print("{s}: Unexpected token {s}\n", .{ lexer.loc, lexer.curr });
                return error.ParseError;
            },
        }
    }

    const binaryOperators = [_][]const TokenType{
        &.{.Equals},
        &.{.DoubleEquals},
        &.{ .LessThan, .GreaterThan },
        &.{ .Plus, .Minus },
        &.{ .Star, .Slash, .Percent },
    };

    fn hasPrecedence(token: Token, precedence: usize) bool {
        for (binaryOperators[precedence]) |operator| {
            if (operator == @as(TokenType, token)) {
                return true;
            }
        }
        return false;
    }

    fn parseTerm(allocator: Allocator, lexer: *Lexer, precedence: usize) Error!Expression {
        if (precedence >= binaryOperators.len) return parseFactor(allocator, lexer);
        var operand1 = try parseTerm(allocator, lexer, precedence + 1);
        errdefer operand1.deinit();

        while (hasPrecedence(lexer.curr, precedence)) {
            var operator = @as(TokenType, lexer.curr);
            _ = lexer.next();
            var operand2 = try parseTerm(allocator, lexer, precedence + 1);
            errdefer operand2.deinit();
            var op1 = try allocator.create(Expression);
            errdefer allocator.destroy(op1);
            op1.* = operand1;
            var op2 = try allocator.create(Expression);
            op2.* = operand2;
            operand1 = Expression{
                .BinaryOp = .{
                    .operand1 = op1,
                    .operand2 = op2,
                    .operator = operator,
                    .allocator = allocator,
                },
            };
        }

        return operand1;
    }

    fn parse(allocator: Allocator, lexer: *Lexer) !Expression {
        return parseTerm(allocator, lexer, 0);
    }

    fn deinit(self: *Expression) void {
        switch (self.*) {
            .Number => {},
            .Variable => {},
            .FunctionCall => |*call| {
                call.argList.deinit();
            },
            .BinaryOp => |*op| {
                op.operand1.deinit();
                op.allocator.destroy(op.operand1);
                op.operand1 = undefined;
                op.operand2.deinit();
                op.allocator.destroy(op.operand2);
                op.operand2 = undefined;
            },
        }
    }

    pub fn format(
        self: Expression,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .Number => |num| {
                return std.fmt.format(writer, "{}", .{num});
            },
            .Variable => |variable| {
                return std.fmt.format(writer, "{s}", .{variable});
            },
            .FunctionCall => |call| {
                return std.fmt.format(writer, "call {s}({s})", .{ call.name, call.argList });
            },
            .BinaryOp => |op| {
                return std.fmt.format(writer, "({s} {s} {s})", .{ op.operand1, @tagName(op.operator), op.operand2 });
            },
        }
    }
};

// A conditional block can be an if statement or a while loop
pub const ConditionalBlock = struct {
    isLoop: bool,
    condition: Expression,
    body: Block,
    allocator: Allocator,

    const Self = @This();

    fn parse(allocator: Allocator, lexer: *Lexer) !Self {
        var isLoop = lexer.curr == .KeywordWhile;
        if (!isLoop) {
            _ = try expectToken(lexer, .KeywordIf);
        } else {
            _ = lexer.next();
        }
        var condition = try Expression.parse(allocator, lexer);
        errdefer condition.deinit();
        _ = try expectToken(lexer, .CurlyOpen);
        _ = try expectToken(lexer, .NewLine);
        var body = try Block.parse(allocator, lexer);
        errdefer body.deinit();
        _ = try expectToken(lexer, .CurlyClose);
        return Self{
            .isLoop = isLoop,
            .condition = condition,
            .body = body,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.condition.deinit();
        self.condition = undefined;
        self.body.deinit();
        self.body = undefined;
    }
};

pub const FunctionDefinition = struct {
    name: []const u8,
    arguments: ArgumentList,
    body: Block,

    const Self = @This();

    fn parse(allocator: Allocator, lexer: *Lexer) !Self {
        _ = try expectToken(lexer, .KeywordFn);
        var nameToken = try expectToken(lexer, .Identifier);
        _ = try expectToken(lexer, .ParenOpen);
        var arguments = try ArgumentList.parse(allocator, lexer);
        errdefer arguments.deinit();
        _ = try expectToken(lexer, .ParenClose);
        _ = try expectToken(lexer, .CurlyOpen);
        _ = try expectToken(lexer, .NewLine);
        var body = try Block.parse(allocator, lexer);
        errdefer body.deinit();
        _ = try expectToken(lexer, .CurlyClose);

        return Self{
            .name = nameToken.Identifier,
            .arguments = arguments,
            .body = body,
        };
    }

    fn deinit(self: *Self) void {
        self.arguments.deinit();
        self.body.deinit();
    }
};

pub const ReturnStatement = struct {
    value: Expression,

    const Self = @This();

    fn parse(allocator: Allocator, lexer: *Lexer) !Self {
        _ = try expectToken(lexer, .KeywordReturn);
        var value = try Expression.parse(allocator, lexer);
        errdefer value.deinit();

        return Self{ .value = value };
    }

    fn deinit(self: *Self) void {
        self.value.deinit();
    }
};

pub const Statement = union(enum) {
    Declaration: Declaration,
    Expression: Expression,
    ConditionalBlock: ConditionalBlock,
    FunctionDefinition: FunctionDefinition,
    ReturnStatement: ReturnStatement,

    fn parse(allocator: Allocator, lexer: *Lexer) Error!Statement {
        var statement: Statement = undefined;
        if (lexer.curr == Token.KeywordVar) {
            var declaration = try Declaration.parse(allocator, lexer);
            statement = Statement{ .Declaration = declaration };
        } else if (lexer.curr == .KeywordWhile or lexer.curr == .KeywordIf) {
            var condBlock = try ConditionalBlock.parse(allocator, lexer);
            statement = Statement{ .ConditionalBlock = condBlock };
        } else if (lexer.curr == .KeywordFn) {
            var fun = try FunctionDefinition.parse(allocator, lexer);
            statement = Statement{ .FunctionDefinition = fun };
        } else if (lexer.curr == .KeywordReturn) {
            var ret = try ReturnStatement.parse(allocator, lexer);
            statement = Statement{ .ReturnStatement = ret };
        } else {
            var expression = try Expression.parse(allocator, lexer);
            statement = Statement{ .Expression = expression };
        }
        errdefer statement.deinit();
        try expectLineEnd(lexer);
        return statement;
    }

    fn deinit(self: *Statement) void {
        switch (self.*) {
            .Declaration => |*decl| {
                decl.deinit();
            },
            .Expression => |*expr| {
                expr.deinit();
            },
            .ConditionalBlock => |*condBlock| {
                condBlock.deinit();
            },
            .FunctionDefinition => |*fun| {
                fun.deinit();
            },
            .ReturnStatement => |*ret| {
                ret.deinit();
            },
        }
    }

    pub fn format(
        self: Statement,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .Declaration => |decl| {
                return std.fmt.format(writer, "{}", .{decl});
            },
            .Expression => |expr| {
                return std.fmt.format(writer, "{}", .{expr});
            },
            .ConditionalBlock => std.debug.todo(""),
            .FunctionDefinition => std.debug.todo(""),
            .ReturnStatement => std.debug.todo(""),
        }
    }
};

pub const Block = struct {
    statements: []Statement,
    allocator: Allocator,

    const Self = @This();

    fn parse(allocator: Allocator, lexer: *Lexer) !Self {
        var statements = std.ArrayList(Statement).init(allocator);
        defer statements.deinit();
        errdefer for (statements.items) |*statement| {
            statement.deinit();
        };

        while (lexer.curr != Token.EoF and lexer.curr != Token.CurlyClose) {
            var statement = try Statement.parse(allocator, lexer);
            errdefer statement.deinit();
            try statements.append(statement);
        }

        return Self{
            .statements = statements.toOwnedSlice(),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.statements) |*statement| {
            statement.deinit();
        }
        self.allocator.free(self.statements);
        self.statements = undefined;
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        for (self.statements) |statement| {
            try std.fmt.format(writer, "{s}\n", .{statement});
        }
    }
};

pub fn parseFile(allocator: Allocator, filename: []const u8, text: []const u8) !Block {
    var lexer = Lexer.init(filename, text);
    _ = lexer.next();
    var block = try Block.parse(allocator, &lexer);
    errdefer block.deinit();
    _ = try expectToken(&lexer, .EoF);
    return block;
}

test "parseProgram" {
    const expectEqual = std.testing.expectEqual;

    const allocator = std.testing.allocator;

    var program =
        \\var i1 = 31 + 1 + 2
        \\var i2 = 35
        \\print(i1 + i2)
        \\print((i1 + 26) * i2)
        \\print(sqrt(i1 + 2) * i2)
    ;

    var block = try parseFile(allocator, "testfile", program);
    defer block.deinit();
    try expectEqual(@as(usize, 5), block.statements.len);
}
