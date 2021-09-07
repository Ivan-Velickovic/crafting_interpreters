const std = @import("std");
const debug = @import("debug.zig");
const stderr = @import("main.zig").stderr;
const VM = @import("vm.zig").VM;
const InterpretError = @import("vm.zig").InterpretError;
const Obj = @import("object.zig").Obj;
const Chunk = @import("chunk.zig").Chunk;
const Value = @import("value.zig").Value;
const Scanner = @import("scanner.zig").Scanner;
const Token = @import("scanner.zig").Token;
const TokenType = @import("scanner.zig").TokenType;
const OpCode = @import("chunk.zig").OpCode;
const maxInt = std.math.maxInt;

// Note that this is only because the Zig compiler (at least in v0.8.0)
// cannot infer recursive error union types, which becomes apparent
// for functions like parsePrecedence.
const CompilerError = error{OutOfMemory} || InterpretError || std.os.WriteError;

pub fn compile(vm: *VM, source: []const u8) !void {
    const scanner = &Scanner.create(source);
    var parser = Parser.create(scanner, vm);

    try parser.advance();

    while (!try parser.match(.EOF)) {
        try parser.declaration();
    }

    try parser.endCompiler();

    if (parser.hadError) return InterpretError.CompileError;
}

const Precedence = enum {
    None,
    Assignment, // =
    Or, // or
    And, // and
    Equality, // == !=
    Comparison, // < > <= >=
    Term, // + -
    Factor, // * /
    Unary, // ! -
    Call, // . ()
    Primary,

    fn next(self: Precedence) Precedence {
        return @intToEnum(Precedence, @enumToInt(self) + 1);
    }
};

fn getPrecedence(tokenType: TokenType) Precedence {
    return switch (tokenType) {
        .Minus, .Plus => .Term,
        .Slash, .Star => .Factor,
        .BangEqual, .EqualEqual => .Equality,
        .Greater, .GreaterEqual, .Less, .LessEqual => .Comparison,
        else => .None,
    };
}

const Parser = struct {
    scanner: *Scanner,
    vm: *VM,
    chunk: *Chunk,
    current: Token,
    previous: Token,
    hadError: bool,
    panicMode: bool,

    fn create(scanner: *Scanner, vm: *VM) Parser {
        return Parser{
            .scanner = scanner,
            .vm = vm,
            .chunk = &vm.chunk,
            .current = undefined,
            .previous = undefined,
            .hadError = false,
            .panicMode = false,
        };
    }

    fn endCompiler(self: *Parser) !void {
        try self.emitReturn();

        const debugPrintCode = @import("build_options").debugPrintCode;
        if (debugPrintCode and !self.hadError) {
            try debug.disassembleChunk(self.chunk, "code");
        }
    }

    fn errorAt(self: *Parser, token: *Token, message: []const u8) !void {
        if (self.panicMode) return;

        self.panicMode = true;
        try stderr.print("[line {}] Error", .{token.line});

        if (token.tokenType == .EOF) {
            try stderr.print(" at end.", .{});
        } else if (token.tokenType == .Error) {
            // Nothing for now.
        } else {
            try stderr.print(" at '{s}'", .{token.lexeme});
        }

        try stderr.print(": {s}\n", .{message});
        self.hadError = true;
    }

    fn errorAtPrevious(self: *Parser, message: []const u8) !void {
        try self.errorAt(&self.previous, message);
    }

    fn errorAtCurrent(self: *Parser, message: []const u8) !void {
        try self.errorAt(&self.current, message);
    }

    fn advance(self: *Parser) !void {
        self.previous = self.current;

        while (true) {
            self.current = self.scanner.scanToken();
            if (self.current.tokenType != .Error) break;

            try self.errorAtCurrent(self.current.lexeme);
        }
    }

    fn consume(self: *Parser, tokenType: TokenType, message: []const u8) !void {
        if (self.current.tokenType == tokenType) {
            try self.advance();
            return;
        }

        try self.errorAtCurrent(message);
    }

    fn check(self: *Parser, tokenType: TokenType) bool {
        return self.current.tokenType == tokenType;
    }

    fn match(self: *Parser, tokenType: TokenType) !bool {
        if (!self.check(tokenType)) return false;
        try self.advance();

        return true;
    }

    fn emitByte(self: *Parser, byte: u8) !void {
        try self.chunk.write(byte, self.previous.line);
    }

    fn emitBytes(self: *Parser, byte1: u8, byte2: u8) !void {
        try self.emitByte(byte1);
        try self.emitByte(byte2);
    }

    fn emitReturn(self: *Parser) !void {
        try self.emitByte(@enumToInt(OpCode.Return));
    }

    fn makeConstant(self: *Parser, value: Value) !u8 {
        const constantIndex = try self.chunk.addConstant(value);
        if (constantIndex > maxInt(u8)) {
            try self.errorAtPrevious("Too many constants in one chunk.");
            return 0;
        }

        return @intCast(u8, constantIndex);
    }

    fn emitConstant(self: *Parser, value: Value) !void {
        try self.emitBytes(@enumToInt(OpCode.Constant), try self.makeConstant(value));
    }

    fn stringValue(self: *Parser, chars: []const u8) !Value {
        const objString = try Obj.String.copy(chars, self.vm);
        return Value.fromObj(&objString.obj);
    }

    fn identifierConstant(self: *Parser, name: []const u8) !u8 {
        return try self.makeConstant(try self.stringValue(name));
    }

    fn binary(self: *Parser) !void {
        const operatorType = self.previous.tokenType;
        const precedence = getPrecedence(operatorType);
        try self.parsePrecedence(precedence.next());

        switch (operatorType) {
            .BangEqual =>       try self.emitBytes(@enumToInt(OpCode.Equal), @enumToInt(OpCode.Not)),
            .EqualEqual =>      try self.emitByte(@enumToInt(OpCode.Equal)),
            .Greater =>         try self.emitByte(@enumToInt(OpCode.Greater)),
            .GreaterEqual =>    try self.emitBytes(@enumToInt(OpCode.Less), @enumToInt(OpCode.Not)),
            .Less =>            try self.emitByte(@enumToInt(OpCode.Less)),
            .LessEqual =>       try self.emitBytes(@enumToInt(OpCode.Greater), @enumToInt(OpCode.Not)),
            .Plus =>            try self.emitByte(@enumToInt(OpCode.Add)),
            .Minus =>           try self.emitByte(@enumToInt(OpCode.Subtract)),
            .Star =>            try self.emitByte(@enumToInt(OpCode.Multiply)),
            .Slash =>           try self.emitByte(@enumToInt(OpCode.Divide)),
            else => unreachable,
        }
    }

    fn literal(self: *Parser) !void {
        switch (self.previous.tokenType) {
            .False => try self.emitByte(@enumToInt(OpCode.False)),
            .Nil => try self.emitByte(@enumToInt(OpCode.Nil)),
            .True => try self.emitByte(@enumToInt(OpCode.True)),
            else => unreachable,
        }
    }

    fn grouping(self: *Parser) !void {
        try self.expression();
        try self.consume(.RightParen, "Expect ')' after expression.");
    }

    fn number(self: *Parser) !void {
        if (std.fmt.parseFloat(f64, self.previous.lexeme)) |value| {
            try self.emitConstant(Value.fromNumber(value));
        } else |e| {
            try self.errorAtPrevious("Could not parse number");
        }
    }

    fn string(self: *Parser) !void {
        // Trim leading and trailing quotation marks.
        const stringToCopy = self.previous.lexeme[1 .. self.previous.lexeme.len - 1];
        try self.emitConstant(try self.stringValue(stringToCopy));
    }

    fn namedVariable(self: *Parser, name: []const u8, canAssign: bool) !void {
        const arg = try self.identifierConstant(name);

        if (canAssign and try self.match(.Equal)) {
            try self.expression();
            try self.emitBytes(@enumToInt(OpCode.SetGlobal), arg);
        } else {
            try self.emitBytes(@enumToInt(OpCode.GetGlobal), arg);
        }
    }

    fn variable(self: *Parser, canAssign: bool) !void {
        try self.namedVariable(self.previous.lexeme, canAssign);
    }

    fn unary(self: *Parser) !void {
        const operatorType = self.previous.tokenType;

        // Compile the operand
        try self.parsePrecedence(.Unary);

        // Emit the operator's corresponding instruction.
        switch (operatorType) {
            .Bang => try self.emitByte(@enumToInt(OpCode.Not)),
            .Minus => try self.emitByte(@enumToInt(OpCode.Negate)),
            else => unreachable,
        }
    }

    fn prefix(self: *Parser, tokenType: TokenType, canAssign: bool) !void {
        switch (tokenType) {
            .LeftParen => try self.grouping(),
            .Minus, .Bang => try self.unary(),
            .Identifier => try self.variable(canAssign),
            .String => try self.string(),
            .Number => try self.number(),
            .False, .True, .Nil => try self.literal(),
            else => try self.prefixError(),
        }
    }

    fn prefixError(self: *Parser) !void {
        try self.errorAtPrevious("Expect expression.");
    }

    fn infix(self: *Parser, tokenType: TokenType, canAssign: bool) !void {
        switch (tokenType) {
            .Minus, .Plus,
            .Slash, .Star,
            .BangEqual, .EqualEqual,
            .Greater, .GreaterEqual, .Less, .LessEqual => try self.binary(),
            else => try self.infixError(),
        }
    }

    fn infixError(self: *Parser) !void {
        try self.errorAtPrevious("Expect expression.");
    }

    fn parsePrecedence(self: *Parser, precedence: Precedence) CompilerError!void {
        try self.advance();

        const canAssign = @enumToInt(precedence) <= @enumToInt(Precedence.Assignment);
        try self.prefix(self.previous.tokenType, canAssign);

        while (@enumToInt(precedence) <= @enumToInt(getPrecedence(self.current.tokenType))) {
            try self.advance();
            try self.infix(self.previous.tokenType, canAssign);
        }

        if (canAssign and try self.match(.Equal)) {
            try self.errorAtPrevious("Invalid assignment target.");
        }
    }

    fn parseVariable(self: *Parser, errorMessage: []const u8) !u8 {
        try self.consume(.Identifier, errorMessage);
        return self.identifierConstant(self.previous.lexeme);
    }

    fn defineVariable(self: *Parser, global: u8) !void {
        try self.emitBytes(@enumToInt(OpCode.DefineGlobal), global);
    }

    fn expression(self: *Parser) !void {
        try self.parsePrecedence(.Assignment);
    }

    fn varDeclaration(self: *Parser) !void {
        const global = try self.parseVariable("Expect variable name.");

        if (try self.match(.Equal)) {
            try self.expression();
        } else {
            try self.emitByte(@enumToInt(OpCode.Nil));
        }
        try self.consume(.Semicolon, "Expect ';' after variable declaration.");

        try self.defineVariable(global);
    }

    fn expressionStatement(self: *Parser) !void {
        try self.expression();
        try self.consume(.Semicolon, "Expect ';' after expression.");
        try self.emitByte(@enumToInt(OpCode.Pop));
    }

    fn printStatement(self: *Parser) !void {
        try self.expression();
        try self.consume(.Semicolon, "Expect ';' after value.");
        try self.emitByte(@enumToInt(OpCode.Print));
    }

    fn synchronize(self: *Parser) !void {
        self.panicMode = false;

        while (self.current.tokenType != .EOF) {
            if (self.previous.tokenType == .Semicolon) return;

            switch (self.current.tokenType) {
                .Class, .Fn, .Var, .For, .If, .While, .Print, .Return => return,
                else => try self.advance(),
            }
        }
    }

    fn declaration(self: *Parser) !void {
        if (try self.match(.Var)) {
            try self.varDeclaration();
        } else {
            try self.statement();
        }

        if (self.panicMode) try self.synchronize();
    }

    fn statement(self: *Parser) !void {
        if (try self.match(.Print)) {
            try self.printStatement();
        } else {
            try self.expressionStatement();
        }
    }
};
