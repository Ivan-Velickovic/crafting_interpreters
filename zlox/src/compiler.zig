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

// Note that this is only because the Zig compiler (at least in v0.8.0)
// cannot infer recursive error union types, which becomes apparent
// for functions like parsePrecedence.
const CompilerError = error{OutOfMemory} || InterpretError || std.os.WriteError;

pub fn compile(vm: *VM, source: []const u8) !void {
    var scanner = Scanner.create(source);
    var compiler = Compiler.create();
    var parser = Parser.create(&scanner, &compiler, vm);

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
        .Or => .Or,
        .And => .And,
        .Minus, .Plus => .Term,
        .Slash, .Star => .Factor,
        .BangEqual, .EqualEqual => .Equality,
        .Greater, .GreaterEqual, .Less, .LessEqual => .Comparison,
        else => .None,
    };
}

const Local = struct {
    name: []const u8,
    depth: isize,
};

const Compiler = struct {
    const MAX_LOCALS = std.math.maxInt(u8) + 1;

    locals: [MAX_LOCALS]Local,
    localCount: u16,
    scopeDepth: u16,

    fn create() Compiler {
        return Compiler{
            .locals = undefined,
            .localCount = 0,
            .scopeDepth = 0,
        };
    }
};

const Parser = struct {
    scanner: *Scanner,
    compiler: *Compiler,
    vm: *VM,
    chunk: *Chunk,
    current: Token,
    previous: Token,
    hadError: bool,
    panicMode: bool,

    fn create(scanner: *Scanner, compiler: *Compiler, vm: *VM) Parser {
        return Parser{
            .scanner = scanner,
            .compiler = compiler,
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

    fn beginScope(self: *Parser) void {
        self.compiler.scopeDepth += 1;
    }

    fn endScope(self: *Parser) !void {
        self.compiler.scopeDepth -= 1;

        while (self.compiler.localCount > 0
                and self.compiler.locals[self.compiler.localCount - 1].depth > self.compiler.scopeDepth) {
            try self.emitOp(.Pop);
            self.compiler.localCount -= 1;
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

    fn emitOp(self: *Parser, op: OpCode) !void {
        try self.emitByte(@enumToInt(op));
    }

    fn emitOps(self: *Parser, op1: OpCode, op2: OpCode) !void {
        try self.emitBytes(@enumToInt(op1), @enumToInt(op2));
    }

    fn emitUnaryOp(self: *Parser, op: OpCode, byte: u8) !void {
        try self.emitOp(op);
        try self.emitByte(byte);
    }

    fn emitJump(self: *Parser, op: OpCode) !usize {
        try self.emitOp(op);
        try self.emitByte(0xff);
        try self.emitByte(0xff);

        return self.chunk.code.items.len - 2;
    }

    fn emitLoop(self: *Parser, loopStart: usize) !void {
        try self.emitOp(.Loop);

        const offset = self.chunk.code.items.len - loopStart + 2;
        if (offset > std.math.maxInt(u16)) {
            try self.errorAtPrevious("Loop body too large.");
        }

        try self.emitByte(@intCast(u8, (offset >> 8) & 0xff));
        try self.emitByte(@intCast(u8, offset & 0xff));
    }

    fn emitReturn(self: *Parser) !void {
        try self.emitOp(.Return);
    }

    fn makeConstant(self: *Parser, value: Value) !u8 {
        const constantIndex = try self.chunk.addConstant(value);
        if (constantIndex > std.math.maxInt(u8)) {
            try self.errorAtPrevious("Too many constants in one chunk.");
            return 0;
        }

        return @intCast(u8, constantIndex);
    }

    fn emitConstant(self: *Parser, value: Value) !void {
        try self.emitUnaryOp(.Constant, try self.makeConstant(value));
    }

    fn patchJump(self: *Parser, offset: usize) !void {
        // We minus 2 for the bytecode for the jump offset.
        const jump = self.chunk.code.items.len - offset - 2;

        if (jump > std.math.maxInt(u16)) {
            try self.errorAtPrevious("Too much code to jump over.");
        }

        self.chunk.code.items[offset] = @intCast(u8, (jump >> 8) & 0xff);
        self.chunk.code.items[offset + 1] = @intCast(u8, jump & 0xff);
    }

    fn stringValue(self: *Parser, chars: []const u8) !Value {
        const objString = try Obj.String.copy(chars, self.vm);
        return Value.fromObj(&objString.obj);
    }

    fn identifierConstant(self: *Parser, name: []const u8) !u8 {
        return try self.makeConstant(try self.stringValue(name));
    }

    fn resolveLocal(self: *Parser, name: []const u8) !isize {
        if (self.compiler.localCount == 0) return -1;

        var i: usize = self.compiler.localCount - 1;
        while (i >= 0) : (i -= 1) {
            const local = self.compiler.locals[i];
            if (std.mem.eql(u8, name, local.name)) {
                if (local.depth == -1) {
                    try self.errorAtPrevious("Can't read local variable in its own initializer.");
                }

                return @intCast(isize, i);
            }
        }

        return -1;
    }

    fn addLocal(self: *Parser, name: []const u8) !void {
        if (self.compiler.locals.len == self.compiler.localCount) {
            try self.errorAtPrevious("Too many local variables in function.");
            return;
        }

        self.compiler.locals[self.compiler.localCount] = Local{
            .name = name,
            .depth = -1,
        };
        self.compiler.localCount += 1;
    }

    fn declareVariable(self: *Parser) !void {
        if (self.compiler.scopeDepth == 0) return;

        if (self.compiler.localCount > 0) {
            // Look for any variables in the same scope that have the same name.
            // We start from end of the array since that is where the current scope is.
            var i: isize = self.compiler.localCount - 1;
            while (i >= 0) : (i -= 1) {
                const local = &self.compiler.locals[@intCast(usize, i)];
                if (local.depth != -1 and local.depth < self.compiler.scopeDepth) break;

                if (std.mem.eql(u8, self.previous.lexeme, local.name)) {
                    try self.errorAtPrevious("Already a variable with this name in this scope.");
                }
            }
        }

        try self.addLocal(self.previous.lexeme);
    }

    fn binary(self: *Parser) !void {
        const operatorType = self.previous.tokenType;
        const precedence = getPrecedence(operatorType);
        try self.parsePrecedence(precedence.next());

        switch (operatorType) {
            .BangEqual =>       try self.emitOps(.Equal, .Not),
            .EqualEqual =>      try self.emitOp(.Equal),
            .Greater =>         try self.emitOp(.Greater),
            .GreaterEqual =>    try self.emitOps(.Less, .Not),
            .Less =>            try self.emitOp(.Less),
            .LessEqual =>       try self.emitOps(.Greater, .Not),
            .Plus =>            try self.emitOp(.Add),
            .Minus =>           try self.emitOp(.Subtract),
            .Star =>            try self.emitOp(.Multiply),
            .Slash =>           try self.emitOp(.Divide),
            else => unreachable,
        }
    }

    fn literal(self: *Parser) !void {
        switch (self.previous.tokenType) {
            .False => try self.emitOp(.False),
            .Nil => try self.emitOp(.Nil),
            .True => try self.emitOp(.True),
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

    fn or_(self: *Parser) !void {
        const elseJump = try self.emitJump(.JumpIfFalse);
        const endJump = try self.emitJump(.Jump);

        try self.patchJump(elseJump);
        try self.emitOp(.Pop);

        try self.parsePrecedence(.Or);
        try self.patchJump(endJump);
    }

    fn string(self: *Parser) !void {
        // Trim leading and trailing quotation marks.
        const stringToCopy = self.previous.lexeme[1 .. self.previous.lexeme.len - 1];
        try self.emitConstant(try self.stringValue(stringToCopy));
    }

    fn namedVariable(self: *Parser, name: []const u8, canAssign: bool) !void {
        var getOp: OpCode = undefined;
        var setOp: OpCode = undefined;
        var arg: u8 = undefined;

        var resolveLocalArg = try self.resolveLocal(name);
        if (resolveLocalArg != -1) {
            arg = @intCast(u8, resolveLocalArg);
            getOp = .GetLocal;
            setOp = .SetLocal;
        } else {
            arg = try self.identifierConstant(name);
            getOp = .GetGlobal;
            setOp = .SetGlobal;
        }

        if (canAssign and try self.match(.Equal)) {
            try self.expression();
            try self.emitUnaryOp(setOp, arg);
        } else {
            try self.emitUnaryOp(getOp, arg);
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
            .Bang => try self.emitOp(.Not),
            .Minus => try self.emitOp(.Negate),
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
            .Or => try self.or_(),
            .And => try self.and_(),
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

        try self.declareVariable();
        if (self.compiler.scopeDepth > 0) return 0;

        return try self.identifierConstant(self.previous.lexeme);
    }

    fn markInitialized(self: *Parser) void {
        self.compiler.locals[self.compiler.localCount - 1].depth = self.compiler.scopeDepth;
    }

    fn defineVariable(self: *Parser, global: u8) !void {
        if (self.compiler.scopeDepth > 0) {
            self.markInitialized();
            return;
        }

        try self.emitUnaryOp(.DefineGlobal, global);
    }

    fn and_(self: *Parser) !void {
        const endJump = try self.emitJump(.JumpIfFalse);

        try self.emitOp(.Pop);
        try self.parsePrecedence(.And);

        try self.patchJump(endJump);
    }

    fn expression(self: *Parser) !void {
        try self.parsePrecedence(.Assignment);
    }

    fn block(self: *Parser) !void {
        while (!self.check(.RightBrace) and !self.check(.EOF)) {
            try self.declaration();
        }

        try self.consume(.RightBrace, "Expect '}' after block.");
    }

    fn varDeclaration(self: *Parser) !void {
        const global = try self.parseVariable("Expect variable name.");

        if (try self.match(.Equal)) {
            try self.expression();
        } else {
            try self.emitOp(.Nil);
        }
        try self.consume(.Semicolon, "Expect ';' after variable declaration.");

        try self.defineVariable(global);
    }

    fn expressionStatement(self: *Parser) !void {
        try self.expression();
        try self.consume(.Semicolon, "Expect ';' after expression.");
        try self.emitOp(.Pop);
    }

    fn forStatement(self: *Parser) !void {
        self.beginScope();

        try self.consume(.LeftParen, "Expect '(' after 'for'.");
        if (try self.match(.Semicolon)) {
            // No initializer.
        } else if (try self.match(.Var)) {
            try self.varDeclaration();
        } else {
            try self.expressionStatement();
        }

        var loopStart = self.chunk.code.items.len;
        var maybeExitJump: ?usize = null;
        if (!try self.match(.Semicolon)) {
            try self.expression();
            try self.consume(.Semicolon, "Expect ';' after loop condition.");

            // Jump out of the loop if the condition is false.
            maybeExitJump = @intCast(usize, try self.emitJump(.JumpIfFalse));
            try self.emitOp(.Pop);
        }

        if (!try self.match(.RightParen)) {
            const bodyJump = try self.emitJump(.Jump);
            const incrementStart = self.chunk.code.items.len;
            try self.expression();
            try self.emitOp(.Pop);
            try self.consume(.RightParen, "Expect ')' after for clauses.");

            try self.emitLoop(loopStart);
            loopStart = incrementStart;
            try self.patchJump(bodyJump);
        }

        try self.statement();
        try self.emitLoop(loopStart);

        if (maybeExitJump) |exitJump| {
            try self.patchJump(exitJump);
            try self.emitOp(.Pop);
        }

        try self.endScope();
    }

    fn ifStatement(self: *Parser) !void {
        try self.consume(.LeftParen, "Expect '(' after 'if'.");
        try self.expression();
        try self.consume(.RightParen, "Expect ')' after condition.");

        const thenJump = try self.emitJump(.JumpIfFalse);
        try self.emitOp(.Pop);
        try self.statement();

        const elseJump = try self.emitJump(.Jump);

        try self.patchJump(thenJump);
        try self.emitOp(.Pop);

        if (try self.match(.Else)) try self.statement();

        try self.patchJump(elseJump);
    }

    fn printStatement(self: *Parser) !void {
        try self.expression();
        try self.consume(.Semicolon, "Expect ';' after value.");
        try self.emitOp(.Print);
    }

    fn whileStatement(self: *Parser) !void {
        const loopStart = self.chunk.code.items.len;
        try self.consume(.LeftParen, "Expect '(' after 'while'.");
        try self.expression();
        try self.consume(.RightParen, "Expect ')' after condition.");

        const exitJump = try self.emitJump(.JumpIfFalse);
        try self.emitOp(.Pop);
        try self.statement();
        try self.emitLoop(loopStart);

        try self.patchJump(exitJump);
        try self.emitOp(.Pop);
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

    fn declaration(self: *Parser) CompilerError!void {
        if (try self.match(.Var)) {
            try self.varDeclaration();
        } else {
            try self.statement();
        }

        if (self.panicMode) try self.synchronize();
    }

    fn statement(self: *Parser) CompilerError!void {
        if (try self.match(.Print)) {
            try self.printStatement();
        } else if (try self.match(.For)) {
            try self.forStatement();
        } else if (try self.match(.If)) {
            try self.ifStatement();
        } else if (try self.match(.While)) {
            try self.whileStatement();
        } else if (try self.match(.LeftBrace)) {
            self.beginScope();
            try self.block();
            try self.endScope();
        } else {
            try self.expressionStatement();
        }
    }
};
