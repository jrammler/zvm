const std = @import("std");
const ascii = std.ascii;

pub const Location = struct {
    filename: []const u8,
    line: usize = 1,
    column: usize = 1,

    const Self = @This();

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try std.fmt.format(writer, "{s}:{}:{}", .{ self.filename, self.line, self.column });
    }
};

pub const Token = union(enum) {
    Identifier: []const u8,
    Number: i32,
    KeywordVar,
    KeywordWhile,
    KeywordIf,
    Equals,
    DoubleEquals,
    LessThan,
    GreaterThan,
    ParenOpen,
    ParenClose,
    CurlyOpen,
    CurlyClose,
    Comma,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    NewLine,
    Init,
    EoF,
    LexError,
};

pub const TokenType = @typeInfo(Token).Union.tag_type.?;

pub const Lexer = struct {
    filename: []const u8,
    text: []const u8,
    pos: usize = 0,
    line: usize = 1,
    col: usize = 1,
    curr: Token = Token.Init,
    loc: Location,

    const LexState = enum {
        Start,
        Identifier,
        Number,
        Equals,
        NewLine,
    };

    const Self = @This();

    pub fn init(filename: []const u8, text: []const u8) Lexer {
        return .{
            .filename = filename,
            .text = text,
            .loc = .{ .filename = filename },
        };
    }

    fn setLocation(self: *Self) void {
        self.loc.line = self.line;
        self.loc.column = self.col;
    }

    pub fn next(self: *Self) Token {
        var state = LexState.Start;
        var startPos: usize = undefined;
        var curPos = self.pos;

        while (true) {
            var c = if (curPos < self.text.len) self.text[curPos] else ascii.control_code.EOT;
            switch (state) {
                .Start => {
                    if (ascii.isAlpha(c)) {
                        state = .Identifier;
                        startPos = curPos;
                        self.setLocation();
                    } else if (ascii.isDigit(c)) {
                        state = .Number;
                        startPos = curPos;
                        self.setLocation();
                    } else if (c == '=') {
                        state = .Equals;
                        startPos = curPos;
                        self.setLocation();
                    } else if (c == '<') {
                        self.pos = curPos + 1;
                        self.curr = .LessThan;
                        self.setLocation();
                        self.col += 1;
                        return self.curr;
                    } else if (c == '>') {
                        self.pos = curPos + 1;
                        self.curr = .GreaterThan;
                        self.setLocation();
                        self.col += 1;
                        return self.curr;
                    } else if (c == '(') {
                        self.pos = curPos + 1;
                        self.curr = .ParenOpen;
                        self.setLocation();
                        self.col += 1;
                        return self.curr;
                    } else if (c == ')') {
                        self.pos = curPos + 1;
                        self.curr = .ParenClose;
                        self.setLocation();
                        self.col += 1;
                        return self.curr;
                    } else if (c == '{') {
                        self.pos = curPos + 1;
                        self.curr = .CurlyOpen;
                        self.setLocation();
                        self.col += 1;
                        return self.curr;
                    } else if (c == '}') {
                        self.pos = curPos + 1;
                        self.curr = .CurlyClose;
                        self.setLocation();
                        self.col += 1;
                        return self.curr;
                    } else if (c == ',') {
                        self.pos = curPos + 1;
                        self.curr = .Comma;
                        self.setLocation();
                        self.col += 1;
                        return self.curr;
                    } else if (c == '+') {
                        self.pos = curPos + 1;
                        self.curr = .Plus;
                        self.setLocation();
                        self.col += 1;
                        return self.curr;
                    } else if (c == '-') {
                        self.pos = curPos + 1;
                        self.curr = .Minus;
                        self.setLocation();
                        self.col += 1;
                        return self.curr;
                    } else if (c == '*') {
                        self.pos = curPos + 1;
                        self.curr = .Star;
                        self.setLocation();
                        self.col += 1;
                        return self.curr;
                    } else if (c == '/') {
                        self.pos = curPos + 1;
                        self.curr = .Slash;
                        self.setLocation();
                        self.col += 1;
                        return self.curr;
                    } else if (c == '%') {
                        self.pos = curPos + 1;
                        self.curr = .Percent;
                        self.setLocation();
                        self.col += 1;
                        return self.curr;
                    } else if (c == ascii.control_code.EOT) {
                        self.pos = curPos;
                        self.curr = .EoF;
                        self.setLocation();
                        return self.curr;
                    } else if (c == ascii.control_code.LF) {
                        state = .NewLine;
                        self.line += 1;
                        self.col = 0;
                        startPos = curPos;
                        self.setLocation();
                    } else if (!ascii.isBlank(c)) {
                        self.curr = .LexError;
                        self.setLocation();
                        return self.curr;
                    }
                },
                .Identifier => {
                    if (!ascii.isAlNum(c)) {
                        self.pos = curPos;
                        var text = self.text[startPos..curPos];
                        if (std.mem.eql(u8, "var", text)) {
                            self.curr = .KeywordVar;
                            return self.curr;
                        } else if (std.mem.eql(u8, "while", text)) {
                            self.curr = .KeywordWhile;
                            return self.curr;
                        } else if (std.mem.eql(u8, "if", text)) {
                            self.curr = .KeywordIf;
                            return self.curr;
                        }
                        self.curr = .{ .Identifier = text };
                        return self.curr;
                    }
                },
                .Number => {
                    if (!ascii.isDigit(c)) {
                        self.pos = curPos;
                        var text = self.text[startPos..curPos];
                        self.curr = .{ .Number = std.fmt.parseInt(i32, text, 0) catch unreachable };
                        return self.curr;
                    }
                },
                .Equals => {
                    if (c != '=') {
                        self.pos = curPos;
                        self.curr = .Equals;
                        return self.curr;
                    }
                    self.pos = curPos + 1;
                    self.col += 1;
                    self.curr = .DoubleEquals;
                    return self.curr;
                },
                .NewLine => {
                    if (c != ascii.control_code.CR and c != ascii.control_code.LF and !ascii.isBlank(c)) {
                        self.pos = curPos;
                        self.curr = .NewLine;
                        return self.curr;
                    } else if (c == ascii.control_code.LF) {
                        self.line += 1;
                        self.col = 0;
                    }
                },
            }
            curPos += 1;
            self.col += 1;
        }
    }

    pub fn isLineEnd(self: *Self) bool {
        return self.curr == .NewLine or self.curr == .EoF;
    }
};

test "lexer" {
    const expectEqual = std.testing.expectEqual;
    const expectEqualSlices = std.testing.expectEqualSlices;

    var lexer = Lexer.init("testfile", "var asdf = 123\n");

    try expectEqual(Token.KeywordVar, lexer.next());
    try expectEqualSlices(u8, "asdf", lexer.next().Identifier);
    try expectEqual(Token.Equals, lexer.next());
    try expectEqual(@as(i32, 123), lexer.next().Number);
    try expectEqual(Token.NewLine, lexer.next());
    try expectEqual(Token.EoF, lexer.next());
    try expectEqual(Token.EoF, lexer.next());
}
