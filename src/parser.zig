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
    std.debug.print("{s}: Expected token {s} but got {s}\n", .{ lexer.loc, tokenType, lexer.curr });
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
        while (lexer.curr != Token.ParenClose) {
            var expr = try Expression.parse(allocator, lexer);
            errdefer expr.deinit();

            try arguments.append(expr);
            if (lexer.curr != TokenType.Comma) break;
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
    Sum: struct {
        summand1: *Expression,
        summand2: *Expression,
        allocator: Allocator,
    },
    Product: struct {
        factor1: *Expression,
        factor2: *Expression,
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

    fn parseTerm(allocator: Allocator, lexer: *Lexer) !Expression {
        var factor1 = try parseFactor(allocator, lexer);
        errdefer factor1.deinit();

        while (lexer.curr == Token.Star) {
            _ = lexer.next();
            var factor2 = try parseFactor(allocator, lexer);
            errdefer factor2.deinit();
            var f1 = try allocator.create(Expression);
            errdefer allocator.destroy(f1);
            f1.* = factor1;
            var f2 = try allocator.create(Expression);
            f2.* = factor2;
            factor1 = Expression{
                .Product = .{
                    .factor1 = f1,
                    .factor2 = f2,
                    .allocator = allocator,
                },
            };
        }

        return factor1;
    }

    fn parse(allocator: Allocator, lexer: *Lexer) Error!Expression {
        var term1 = try parseTerm(allocator, lexer);
        errdefer term1.deinit();

        while (lexer.curr == Token.Plus) {
            _ = lexer.next();
            var term2 = try parseTerm(allocator, lexer);
            errdefer term2.deinit();
            var s1 = try allocator.create(Expression);
            errdefer allocator.destroy(s1);
            s1.* = term1;
            var s2 = try allocator.create(Expression);
            s2.* = term2;
            term1 = Expression{
                .Sum = .{
                    .summand1 = s1,
                    .summand2 = s2,
                    .allocator = allocator,
                },
            };
        }

        return term1;
    }

    fn deinit(self: *Expression) void {
        switch (self.*) {
            .Number => {},
            .Variable => {},
            .FunctionCall => |*call| {
                call.argList.deinit();
            },
            .Sum => |*sum| {
                sum.summand1.deinit();
                sum.allocator.destroy(sum.summand1);
                sum.summand1 = undefined;
                sum.summand2.deinit();
                sum.allocator.destroy(sum.summand2);
                sum.summand2 = undefined;
            },
            .Product => |*prod| {
                prod.factor1.deinit();
                prod.allocator.destroy(prod.factor1);
                prod.factor1 = undefined;
                prod.factor2.deinit();
                prod.allocator.destroy(prod.factor2);
                prod.factor2 = undefined;
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
            .Sum => |s| {
                return std.fmt.format(writer, "({s} + {s})", .{ s.summand1, s.summand2 });
            },
            .Product => |p| {
                return std.fmt.format(writer, "({s} * {s})", .{ p.factor1, p.factor2 });
            },
        }
    }
};

pub const Statement = union(enum) {
    Declaration: Declaration,
    Expression: Expression,

    fn parse(allocator: Allocator, lexer: *Lexer) !Statement {
        var statement: Statement = undefined;
        if (lexer.curr == Token.KeywordVar) {
            var declaration = try Declaration.parse(allocator, lexer);
            statement = Statement{ .Declaration = declaration };
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
        }
    }
};

pub const Ast = struct {
    statements: []Statement,
    allocator: Allocator,

    pub fn parse(allocator: Allocator, filename: []const u8, text: []const u8) !Ast {
        var lexer = Lexer.init(filename, text);
        _ = lexer.next();
        var statements = std.ArrayList(Statement).init(allocator);
        defer statements.deinit();

        while (lexer.curr != Token.EoF) {
            var statement = try Statement.parse(allocator, &lexer);
            try statements.append(statement);
        }

        return Ast{
            .statements = statements.toOwnedSlice(),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Ast) void {
        for (self.statements) |*statement| {
            statement.deinit();
        }
        self.allocator.free(self.statements);
        self.statements = undefined;
    }

    pub fn format(
        self: Ast,
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

    var ast = try Ast.parse(allocator, "testfile", program);
    defer ast.deinit();
    try expectEqual(@as(usize, 5), ast.statements.len);
}
