const std = @import("std");

pub const TokenType = enum {
    // Single-character tokens
    LeftParen, RightParen, LeftBrace, RightBrace, Comma, Dot, Minus, Plus, Semicolon, Slash, Star,
    // One or two character tokens
    Bang, BangEqual, Equal, EqualEqual, Greater, GreaterEqual, Less, LessEqual,
    // Literals
    Identifier, String, Number,
    // Keywords
    And, Class, Else, False, For, Fn, If, Nil, Or, Print, Return, Super, This, True, Var, While,
    // Other
    Error, EOF
};

pub const Token = struct {
    tokenType: TokenType,
    lexeme: []const u8,
    line: u32,
};

pub const Scanner = struct {
    start: []const u8,
    current: usize,
    line: u32,

    pub fn create(source: []const u8) Scanner {
        return Scanner{
            .start = source,
            .current = 0,
            .line = 1,
        };
    }

    pub fn scanToken(self: *Scanner) Token {
        self.skipWhitespace();

        self.start = self.start[self.current..];
        self.current = 0;

        if (self.isAtEnd()) return self.makeToken(.EOF);

        const char = self.peek();
        self.advance();

        return switch (char) {
            '(' => self.makeToken(.LeftParen),
            ')' => self.makeToken(.RightParen),
            '{' => self.makeToken(.LeftBrace),
            '}' => self.makeToken(.RightBrace),
            ';' => self.makeToken(.Semicolon),
            ',' => self.makeToken(.Comma),
            '.' => self.makeToken(.Dot),
            '-' => self.makeToken(.Minus),
            '+' => self.makeToken(.Plus),
            '/' => self.makeToken(.Slash),
            '*' => self.makeToken(.Star),
            '!' => self.makeToken(if (self.match('=')) .BangEqual else .Bang),
            '=' => self.makeToken(if (self.match('=')) .EqualEqual else .Equal),
            '<' => self.makeToken(if (self.match('=')) .LessEqual else .Less),
            '>' => self.makeToken(if (self.match('=')) .GreaterEqual else .Greater),
            '"' => self.scanString(),
            else => {
                if (isAlpha(char)) return self.scanIdentifier();
                if (isDigit(char)) return self.scanNumber();
                return self.errorToken("Unexpected character.");
            }
        };
    }

    fn scanIdentifier(self: *Scanner) Token {
        while (isAlpha(self.peek()) or isDigit(self.peek())) self.advance();

        return self.makeToken(self.identifierType());
    }

    fn scanNumber(self: *Scanner) Token {
        while (isDigit(self.peek())) self.advance();

        // Look for a fractional part.
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            self.advance(); // Consume `.`.

            while (isDigit(self.peek())) self.advance();
        }

        return self.makeToken(.Number);
    }

    fn scanString(self: *Scanner) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') self.line += 1;
            self.advance();
        }

        if (self.isAtEnd()) return self.errorToken("Unterminated string.");

        // Need to consume the closing quote `"`.
        self.advance();

        return self.makeToken(.String);
    }

    fn makeToken(self: *Scanner, tokenType: TokenType) Token {
        return Token{
            .tokenType = tokenType,
            .lexeme = self.start[0..self.current],
            .line = self.line,
        };
    }

    fn errorToken(self: *Scanner, message: []const u8) Token {
        return Token{
            .tokenType = .Error,
            .lexeme = message,
            .line = self.line,
        };
    }

    fn isAtEnd(self: *Scanner) bool {
        return self.current >= self.start.len;
    }

    fn advance(self: *Scanner) void {
        self.current += 1;
    }

    fn peek(self: *Scanner) u8 {
        if (self.isAtEnd()) return 0;
        return self.start[self.current];
    }

    fn peekNext(self: *Scanner) u8 {
        if (self.current + 1 >= self.start.len) return 0;
        return self.start[self.current + 1];
    }

    fn match(self: *Scanner, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.peek() != expected) return false;

        self.current += 1;

        return true;
    }

    fn checkKeyword(self: *Scanner, startIndex: u8, rest: []const u8, tokenType: TokenType) TokenType {
        const sourceSlice = self.start[startIndex .. startIndex + rest.len];

        return if (std.mem.eql(u8, sourceSlice, rest)) tokenType else .Identifier;
    }

    fn identifierType(self: *Scanner) TokenType {
        return switch (self.start[0]) {
            'a' => self.checkKeyword(1, "nd", .And),
            'c' => self.checkKeyword(1, "lass", .Class),
            'e' => self.checkKeyword(1, "lse", .Else),
            'f' => {
                if (self.start[0..self.current].len > 1) {
                    return switch (self.start[1]) {
                        'a' => self.checkKeyword(2, "lse", .False),
                        'o' => self.checkKeyword(2, "r", .For),
                        'n' => .Fn,
                        else => .Identifier,
                    };
                } else {
                    return .Identifier;
                }
            },
            'i' => self.checkKeyword(1, "f", .If),
            'n' => self.checkKeyword(1, "il", .Nil),
            'o' => self.checkKeyword(1, "r", .Or),
            'p' => self.checkKeyword(1, "rint", .Print),
            'r' => self.checkKeyword(1, "eturn", .Return),
            's' => self.checkKeyword(1, "uper", .Super),
            't' => {
                if (self.start[0..self.current].len > 1) {
                    return switch (self.start[1]) {
                        'h' => self.checkKeyword(2, "is", .This),
                        'r' => self.checkKeyword(2, "ue", .True),
                        else => .Identifier,
                    };
                } else {
                    return .Identifier;
                }
            },
            'v' => self.checkKeyword(1, "ar", .Var),
            'w' => self.checkKeyword(1, "hile", .While),
            else => .Identifier,
        };
    }

    fn skipWhitespace(self: *Scanner) void {
        while (true) {
            const char = self.peek();
            switch (char) {
                ' ', '\r', '\t' => self.advance(),
                '\n' => {
                    self.line += 1;
                    self.advance();
                },
                '/' => {
                    if (self.peekNext() == '/') {
                        // Comments go until the end of a line.
                        while (self.peek() != '\n' and !self.isAtEnd()) self.advance();
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }
};

fn isAlpha(char: u8) bool {
    return (char >= 'a' and char <= 'z') or
           (char >= 'A' and char <= 'Z') or
           char == '_';
}

fn isDigit(char: u8) bool {
    return char >= '0' and char <= '9';
}
