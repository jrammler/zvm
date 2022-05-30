const std = @import("std");
const Allocator = std.mem.Allocator;
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("lexer.zig").Token;

const Declaration = struct {
    identifier: []const u8,
    value: Expression,

    fn parse(allocator: Allocator, lexer: *Lexer) !?Declaration {
        if (lexer.curr != Token.KeywordVar) {
            return null;
        }
        var nameToken = lexer.next();
        if (nameToken != Token.Identifier) {
            return null;
        }
        if (lexer.next() != Token.Equals) {
            return null;
        }
        _ = lexer.next();
        var initExpr = try Expression.parse(allocator, lexer);
        if (initExpr == null) {
            return null;
        }

        return Declaration{
            .identifier = nameToken.Identifier,
            .value = initExpr.?,
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

    fn parse(allocator: Allocator, lexer: *Lexer) !?ArgumentList {
        var arguments = std.ArrayList(Expression).init(allocator);
        defer arguments.deinit();
        while (try Expression.parse(allocator, lexer)) |expr| {
            try arguments.append(expr);
            if (lexer.curr != Token.Comma) {
                break;
            }
            _ = lexer.next();
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

const Expression = union(enum) {
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

    fn parseFactor(allocator: Allocator, lexer: *Lexer) !?Expression {
        _ = allocator;
        switch (lexer.curr) {
            .Number => {
                defer _ = lexer.next();
                return Expression{ .Number = lexer.curr.Number };
            },
            .Identifier => {
                var nameToken = lexer.curr;
                _ = lexer.next();
                if (lexer.curr != Token.ParenOpen) {
                    return Expression{ .Variable = nameToken.Identifier };
                }
                _ = lexer.next();
                var argList = if (try ArgumentList.parse(allocator, lexer)) |argList| argList else return null;
                if (lexer.curr != Token.ParenClose) {
                    argList.deinit();
                    return null;
                }
                _ = lexer.next();
                return Expression{ .FunctionCall = .{
                    .name = nameToken.Identifier,
                    .argList = argList,
                } };
            },
            .ParenOpen => {
                _ = lexer.next();
                var expr = if (try Expression.parse(allocator, lexer)) |expr| expr else return null;
                if (lexer.curr != Token.ParenClose) {
                    expr.deinit();
                    return null;
                }
                _ = lexer.next();
                return expr;
            },
            else => {
                return null;
            },
        }
    }

    fn parseTerm(allocator: Allocator, lexer: *Lexer) !?Expression {
        var factor1 = if (try parseFactor(allocator, lexer)) |factor| factor else return null;

        while (lexer.curr == Token.Star) {
            _ = lexer.next();
            var factor2 = try parseFactor(allocator, lexer);
            if (factor2 == null) {
                factor1.deinit();
                return null;
            }
            var f1 = try allocator.create(Expression);
            f1.* = factor1;
            var f2 = try allocator.create(Expression);
            f2.* = factor2.?;
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

    fn parse(allocator: Allocator, lexer: *Lexer) Allocator.Error!?Expression {
        var term1 = if (try parseTerm(allocator, lexer)) |term| term else return null;

        while (lexer.curr == Token.Plus) {
            _ = lexer.next();
            var term2 = try parseTerm(allocator, lexer);
            if (term2 == null) {
                term1.deinit();
                return null;
            }
            var s1 = try allocator.create(Expression);
            s1.* = term1;
            var s2 = try allocator.create(Expression);
            s2.* = term2.?;
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

const Statement = union(enum) {
    Declaration: Declaration,
    Expression: Expression,

    fn parse(allocator: Allocator, lexer: *Lexer) !?Statement {
        var statement: Statement = undefined;
        if (try Declaration.parse(allocator, lexer)) |declaration| {
            statement = Statement{ .Declaration = declaration };
        } else if (try Expression.parse(allocator, lexer)) |expression| {
            statement = Statement{ .Expression = expression };
        } else {
            return null;
        }
        if (!lexer.isLineEnd()) {
            statement.deinit();
            return null;
        }
        _ = lexer.next();
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

    pub fn parse(allocator: Allocator, text: []const u8) !?Ast {
        var lexer = Lexer.init(text);
        _ = lexer.next();
        var statements = std.ArrayList(Statement).init(allocator);
        defer statements.deinit();

        while (try Statement.parse(allocator, &lexer)) |statement| {
            try statements.append(statement);
        }
        if (lexer.curr == Token.NewLine) {
            _ = lexer.next();
        }
        if (lexer.curr != Token.EoF) {
            for (statements.items) |*statement| {
                statement.deinit();
            }
            return null;
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
    const expect = std.testing.expect;

    const allocator = std.testing.allocator;

    var program =
        \\var i1 = 31 + 1 + 2
        \\var i2 = 35
        \\print(i1 + i2)
        \\print((i1 + 26) * i2)
        \\print(sqrt(i1 + 2) * i2)
    ;

    var ast = try Ast.parse(allocator, program);
    try expect(ast != null);
    defer ast.?.deinit();
    try expectEqual(@as(usize, 5), ast.?.statements.len);
}
