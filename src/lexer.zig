const std = @import("std");
const ascii = std.ascii;

pub const Token = union(enum) {
    Identifier: []const u8,
    Number: i32,
    KeywordVar,
    Equals,
    ParenOpen,
    ParenClose,
    Comma,
    Plus,
    Star,
    NewLine,
    Init,
    EoF,
    LexError,
};

pub const Lexer = struct {
    text: []const u8,
    pos: usize,
    curr: Token,

    const LexState = enum {
        Start,
        Identifier,
        Number,
        NewLine,
    };

    pub fn init(text: []const u8) Lexer {
        return .{
            .text = text,
            .pos = 0,
            .curr = Token.Init,
        };
    }

    pub fn next(self: *Lexer) Token {
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
                    } else if (ascii.isDigit(c)) {
                        state = .Number;
                        startPos = curPos;
                    } else if (c == '=') {
                        self.pos = curPos + 1;
                        self.curr = .Equals;
                        return self.curr;
                    } else if (c == '(') {
                        self.pos = curPos + 1;
                        self.curr = .ParenOpen;
                        return self.curr;
                    } else if (c == ')') {
                        self.pos = curPos + 1;
                        self.curr = .ParenClose;
                        return self.curr;
                    } else if (c == ',') {
                        self.pos = curPos + 1;
                        self.curr = .Comma;
                        return self.curr;
                    } else if (c == '+') {
                        self.pos = curPos + 1;
                        self.curr = .Plus;
                        return self.curr;
                    } else if (c == '*') {
                        self.pos = curPos + 1;
                        self.curr = .Star;
                        return self.curr;
                    } else if (c == ascii.control_code.EOT) {
                        self.pos = curPos;
                        self.curr = .EoF;
                        return self.curr;
                    } else if (c == ascii.control_code.LF) {
                        state = .NewLine;
                        startPos = curPos;
                    } else if (!ascii.isBlank(c)) {
                        self.curr = .LexError;
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
                .NewLine => {
                    if (c != ascii.control_code.CR and c != ascii.control_code.LF and !ascii.isBlank(c)) {
                        self.pos = curPos;
                        self.curr = .NewLine;
                        return self.curr;
                    }
                },
            }
            curPos += 1;
        }
    }

    pub fn isLineEnd(self: *Lexer) bool {
        return self.curr == .NewLine or self.curr == .EoF;
    }
};

test "lexer" {
    const expectEqual = std.testing.expectEqual;
    const expectEqualSlices = std.testing.expectEqualSlices;

    var lexer = Lexer.init("var asdf = 123\n");

    try expectEqual(Token.KeywordVar, lexer.next());
    try expectEqualSlices(u8, "asdf", lexer.next().Identifier);
    try expectEqual(Token.Equals, lexer.next());
    try expectEqual(@as(i32, 123), lexer.next().Number);
    try expectEqual(Token.NewLine, lexer.next());
    try expectEqual(Token.EoF, lexer.next());
    try expectEqual(Token.EoF, lexer.next());
}
