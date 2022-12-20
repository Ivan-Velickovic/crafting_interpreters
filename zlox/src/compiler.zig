const std = @import("std");
const debug_options = @import("debug_options");
const debug = @import("debug.zig");
const stderr = @import("main.zig").stderr;
const GC = @import("memory.zig").GC;
const VM = @import("vm.zig").VM;
const InterpretError = @import("vm.zig").InterpretError;
const Obj = @import("object.zig").Obj;
const Chunk = @import("chunk.zig").Chunk;
const Value = @import("value.zig").Value;
const Scanner = @import("scanner.zig").Scanner;
const Token = @import("scanner.zig").Token;
const TokenType = @import("scanner.zig").TokenType;
const OpCode = @import("chunk.zig").OpCode;

const Function = Obj.Function;
const String = Obj.String;

// Note that this is only because the Zig compiler (at least in v0.8.0)
// cannot infer recursive error union types, which becomes apparent
// for functions like parsePrecedence.
const CompilerError = InterpretError || std.os.WriteError || std.mem.Allocator.Error;

pub fn compile(source: []const u8, vm: *VM) !*Obj.Function {
    var compiler = try Compiler.create(vm, null, .Script);
    var parser = try Parser.create(source, vm, &compiler);

    try parser.advance();

    while (!try parser.match(.EOF)) {
        try parser.declaration();
    }

    const function = try parser.end();

    return if (!parser.had_error) function else InterpretError.CompileError;
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
        std.debug.assert(self != .Primary);
        return @intToEnum(Precedence, @enumToInt(self) + 1);
    }
};

fn getPrecedence(tokenType: TokenType) Precedence {
    return switch (tokenType) {
        .LeftParen, .Dot => .Call,
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
    is_captured: bool,
    depth: isize,
};

const Upvalue = packed struct {
    index: u8,
    is_local: bool,
};

const FunctionType = enum { Function, Initialiser, Method, Script };

pub const Compiler = struct {
    pub const MAX_LOCALS = std.math.maxInt(u8) + 1;
    pub const MAX_UPVALUES = std.math.maxInt(u8) + 1;

    enclosing: ?*Compiler,
    function: *Obj.Function,
    function_type: FunctionType,
    locals: [MAX_LOCALS]Local,
    local_count: u9,
    upvalues: [MAX_UPVALUES]Upvalue,
    scope_depth: u9,

    fn create(vm: *VM, enclosing: ?*Compiler, function_type: FunctionType) !Compiler {
        var compiler = Compiler{
            .enclosing = enclosing,
            .function = try Obj.Function.create(vm),
            .function_type = function_type,
            .locals = undefined,
            .upvalues = undefined,
            .local_count = 0,
            .scope_depth = 0,
        };

        // Here we define the first slot.
        // In the case of method calls, we use this slot for the "this" keyword/variable.
        // In this case of function calls, we use this slot for the function being called.
        const name = if (function_type != .Function) "this" else "";
        compiler.locals[0] = Local{
            .name = name,
            .is_captured = false,
            .depth = 0,
        };
        compiler.local_count = 1;

        return compiler;
    }
};

const ClassCompiler = struct {
    enclosing: ?*ClassCompiler,
    has_superclass: bool = false,
};

pub const Parser = struct {
    vm: *VM,
    compiler: *Compiler,
    scanner: Scanner,
    current: Token,
    current_class: ?*ClassCompiler,
    previous: Token,
    had_error: bool,
    panic_mode: bool,

    fn create(source: []const u8, vm: *VM, compiler: *Compiler) !Parser {
        return Parser{
            .vm = vm,
            .compiler = compiler,
            .scanner = Scanner.create(source),
            .current = undefined,
            .current_class = null,
            .previous = undefined,
            .had_error = false,
            .panic_mode = false,
        };
    }

    fn end(self: *Parser) !*Obj.Function {
        try self.emitReturn();
        const function = self.compiler.function;

        if (debug_options.print_code and !self.had_error) {
            const chunk_name = if (function.name) |name| name.chars else "<script>";
            try debug.disassembleChunk(self.currentChunk(), chunk_name);
        }

        // To avoid having an optional compiler pointer in the parser, we instead only assign
        // the enclosing compiler when there actually is an enclosing compiler (i.e. the
        // compiler being "ended" isn't the top-level one). The reason this works is because
        // when the top-level compiler is "ended", it is not referred to any more by the parser.
        if (self.compiler.enclosing) |enclosing| {
            self.compiler = enclosing;
        }

        return function;
    }

    fn currentChunk(self: *Parser) *Chunk {
        return &self.compiler.function.chunk;
    }

    fn beginScope(self: *Parser) void {
        self.compiler.scope_depth += 1;
    }

    fn endScope(self: *Parser) !void {
        self.compiler.scope_depth -= 1;

        while (self.compiler.local_count > 0 and self.compiler.locals[self.compiler.local_count - 1].depth > self.compiler.scope_depth) {
            if (self.compiler.locals[self.compiler.local_count - 1].is_captured) {
                try self.emitOp(.CloseUpvalue);
            } else {
                try self.emitOp(.Pop);
            }
            self.compiler.local_count -= 1;
        }
    }

    fn errorAt(self: *Parser, token: *Token, comptime fmt: []const u8, args: anytype) !void {
        if (self.panic_mode) return;

        self.panic_mode = true;
        try stderr.print("[line {}] Error", .{ token.line });

        if (token.tokenType == .EOF) {
            try stderr.print(" at end", .{});
        } else if (token.tokenType == .Error) {
            // Nothing for now.
        } else {
            try stderr.print(" at '{s}'", .{ token.lexeme });
        }

        try stderr.print(": " ++ fmt ++ "\n", args);
        self.had_error = true;
    }

    fn errorAtPrevious(self: *Parser, comptime fmt: []const u8, args: anytype) !void {
        try self.errorAt(&self.previous, fmt, args);
    }

    fn errorAtCurrent(self: *Parser, comptime fmt: []const u8, args: anytype) !void {
        try self.errorAt(&self.current, fmt, args);
    }

    fn advance(self: *Parser) !void {
        self.previous = self.current;

        while (true) {
            self.current = self.scanner.scanToken();
            if (self.current.tokenType != .Error) break;

            try self.errorAtCurrent("{s}", .{ self.current.lexeme });
        }
    }

    fn consume(self: *Parser, tokenType: TokenType, comptime msg: []const u8) !void {
        if (self.current.tokenType == tokenType) {
            try self.advance();
            return;
        }

        try self.errorAtCurrent("{s}", .{ msg });
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
        try self.currentChunk().write(byte, self.previous.line);
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

        return self.currentChunk().code.items.len - 2;
    }

    fn emitLoop(self: *Parser, loopStart: usize) !void {
        try self.emitOp(.Loop);

        const offset = self.currentChunk().code.items.len - loopStart + 2;
        if (offset > std.math.maxInt(u16)) {
            try self.errorAtPrevious("Loop body too large.", .{});
        }

        try self.emitByte(@intCast(u8, (offset >> 8) & 0xff));
        try self.emitByte(@intCast(u8, offset & 0xff));
    }

    fn emitReturn(self: *Parser) !void {
        if (self.compiler.function_type == .Initialiser) {
            try self.emitUnaryOp(.GetLocal, 0);
        } else {
            try self.emitOp(.Nil);
        }
        try self.emitOp(.Return);
    }

    fn makeConstant(self: *Parser, value: Value) !u8 {
        const constantIndex = try self.currentChunk().addConstant(self.vm, value);
        if (constantIndex > std.math.maxInt(u8)) {
            try self.errorAtPrevious("Too many constants in one chunk.", .{});
            return 0;
        }

        return @intCast(u8, constantIndex);
    }

    fn emitConstant(self: *Parser, value: Value) !void {
        try self.emitUnaryOp(.Constant, try self.makeConstant(value));
    }

    fn patchJump(self: *Parser, offset: usize) !void {
        // We minus 2 for the bytecode for the jump offset.
        const jump = self.currentChunk().code.items.len - offset - 2;

        if (jump > std.math.maxInt(u16)) {
            try self.errorAtPrevious("Too much code to jump over.", .{});
        }

        self.currentChunk().code.items[offset] = @intCast(u8, (jump >> 8) & 0xff);
        self.currentChunk().code.items[offset + 1] = @intCast(u8, jump & 0xff);
    }

    fn stringValue(self: *Parser, chars: []const u8) !Value {
        const string_obj = try String.copy(self.vm, chars);
        return Value.fromObj(&string_obj.obj);
    }

    fn identifierConstant(self: *Parser, name: []const u8) !u8 {
        return try self.makeConstant(try self.stringValue(name));
    }

    fn resolveLocal(self: *Parser, compiler: *Compiler, name: []const u8) !isize {
        var i: isize = compiler.local_count - 1;
        while (i >= 0) : (i -= 1) {
            const local = compiler.locals[@intCast(usize, i)];
            if (std.mem.eql(u8, name, local.name)) {
                if (local.depth == -1) {
                    try self.errorAtPrevious("Can't read local variable in its own initializer.", .{});
                }

                return i;
            }
        }

        return -1;
    }

    fn addLocal(self: *Parser, name: []const u8) !void {
        if (self.compiler.local_count == Compiler.MAX_LOCALS) {
            try self.errorAtPrevious("Too many local variables in function.", .{});
            return;
        }

        self.compiler.locals[self.compiler.local_count] = Local{
            .name = name,
            .is_captured = false,
            .depth = -1,
        };
        self.compiler.local_count += 1;
    }

    fn resolveUpvalue(self: *Parser, compiler: *Compiler, name: []const u8) CompilerError!isize {
        if (compiler.enclosing) |enclosing| {
            const local = try self.resolveLocal(enclosing, name);
            if (local != -1) {
                enclosing.locals[@intCast(usize, local)].is_captured = true;
                return @intCast(isize, try self.addUpvalue(compiler, @intCast(u8, local), true));
            }

            const upvalue = try self.resolveUpvalue(enclosing, name);
            if (upvalue != -1) {
                return @intCast(isize, try self.addUpvalue(compiler, @intCast(u8, upvalue), false));
            }
        }

        return -1;
    }

    fn addUpvalue(self: *Parser, compiler: *Compiler, index: u8, is_local: bool) !usize {
        var i: usize = 0;
        while (i < compiler.function.upvalueCount) : (i += 1) {
            const upvalue = compiler.upvalues[i];
            if (upvalue.index == index and upvalue.is_local == is_local) {
                return i;
            }
        }

        if (compiler.function.upvalueCount == Compiler.MAX_UPVALUES) {
            try self.errorAtPrevious("Too many closure variables in function.", .{});
            return InterpretError.CompileError;
        }

        compiler.upvalues[compiler.function.upvalueCount] = Upvalue{
            .is_local = is_local,
            .index = index,
        };
        compiler.function.upvalueCount += 1;

        return compiler.function.upvalueCount - 1;
    }

    fn declareVariable(self: *Parser) !void {
        if (self.compiler.scope_depth == 0) return;

        // Look for any variables in the same scope that have the same name.
        // We start from end of the array since that is where the current scope is.
        var i: isize = self.compiler.local_count - 1;
        while (i >= 0) : (i -= 1) {
            const local = self.compiler.locals[@intCast(usize, i)];
            if (local.depth != -1 and local.depth < self.compiler.scope_depth) break;

            if (std.mem.eql(u8, self.previous.lexeme, local.name)) {
                try self.errorAtPrevious("Already a variable with this name in this scope.", .{});
            }
        }

        try self.addLocal(self.previous.lexeme);
    }

    fn binary(self: *Parser) !void {
        const operator_type = self.previous.tokenType;
        const precedence = getPrecedence(operator_type);
        try self.parsePrecedence(precedence.next());

        switch (operator_type) {
            .BangEqual => try self.emitOps(.Equal, .Not),
            .EqualEqual => try self.emitOp(.Equal),
            .Greater => try self.emitOp(.Greater),
            .GreaterEqual => try self.emitOps(.Less, .Not),
            .Less => try self.emitOp(.Less),
            .LessEqual => try self.emitOps(.Greater, .Not),
            .Plus => try self.emitOp(.Add),
            .Minus => try self.emitOp(.Subtract),
            .Star => try self.emitOp(.Multiply),
            .Slash => try self.emitOp(.Divide),
            else => unreachable,
        }
    }

    fn call(self: *Parser) !void {
        const arg_count = try self.argumentList();
        try self.emitUnaryOp(.Call, arg_count);
    }

    fn dot(self: *Parser, can_assign: bool) !void {
        try self.consume(.Identifier, "Expect property name after '.'.");
        const name = try self.identifierConstant(self.previous.lexeme);

        if (can_assign and try self.match(.Equal)) {
            try self.expression();
            try self.emitUnaryOp(.SetProperty, name);
        } else if (try self.match(.LeftParen)) {
            const arg_count = try self.argumentList();
            try self.emitUnaryOp(.Invoke, name);
            try self.emitByte(arg_count);
        } else {
            try self.emitUnaryOp(.GetProperty, name);
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
        } else |err| {
            try self.errorAtPrevious("Could not parse number: {}", .{ err });
        }
    }

    fn or_(self: *Parser) !void {
        const else_jump = try self.emitJump(.JumpIfFalse);
        const end_jump = try self.emitJump(.Jump);

        try self.patchJump(else_jump);
        try self.emitOp(.Pop);

        try self.parsePrecedence(.Or);
        try self.patchJump(end_jump);
    }

    fn string(self: *Parser) !void {
        // Trim leading and trailing quotation marks.
        const string_to_copy = self.previous.lexeme[1 .. self.previous.lexeme.len - 1];
        try self.emitConstant(try self.stringValue(string_to_copy));
    }

    fn namedVariable(self: *Parser, name: []const u8, can_assign: bool) !void {
        var get_op: OpCode = undefined;
        var set_op: OpCode = undefined;
        var arg: u8 = undefined;

        const resolve_local_arg = try self.resolveLocal(self.compiler, name);
        if (resolve_local_arg != -1) {
            arg = @intCast(u8, resolve_local_arg);
            get_op = .GetLocal;
            set_op = .SetLocal;
        } else {
            const resolve_upvalue_arg = try self.resolveUpvalue(self.compiler, name);
            if (resolve_upvalue_arg != -1) {
                arg = @intCast(u8, resolve_upvalue_arg);
                get_op = .GetUpvalue;
                set_op = .SetUpvalue;
            } else {
                arg = try self.identifierConstant(name);
                get_op = .GetGlobal;
                set_op = .SetGlobal;
            }
        }

        if (can_assign and try self.match(.Equal)) {
            try self.expression();
            try self.emitUnaryOp(set_op, arg);
        } else {
            try self.emitUnaryOp(get_op, arg);
        }
    }

    fn variable(self: *Parser, can_assign: bool) !void {
        try self.namedVariable(self.previous.lexeme, can_assign);
    }

    fn super(self: *Parser) !void {
        if (self.current_class == null) {
            try self.errorAtPrevious("Can't use 'super' outside of a class.", .{});
        } else if (!self.current_class.?.has_superclass) {
            try self.errorAtPrevious("Can't use 'super' in a class with no superclass.", .{});
        }

        try self.consume(.Dot, "Expect '.' after 'super'.");
        try self.consume(.Identifier, "Expect superclass method name.");
        const method_name = try self.identifierConstant(self.previous.lexeme);

        try self.namedVariable("this", false);
        if (try self.match(.LeftParen)) {
            const arg_count = try self.argumentList();
            try self.namedVariable("super", false);
            try self.emitUnaryOp(.SuperInvoke, method_name);
            try self.emitByte(arg_count);
        } else {
            try self.namedVariable("super", false);
            try self.emitUnaryOp(.GetSuper, method_name);
        }
    }

    fn this(self: *Parser) !void {
        if (self.current_class) |_| {
            try self.variable(false);
        } else {
            try self.errorAtPrevious("Can't use 'this' outside of a class.", .{});
        }
    }

    fn unary(self: *Parser) !void {
        const operator_type = self.previous.tokenType;
        // Compile the operand
        try self.parsePrecedence(.Unary);
        // Emit the operator's corresponding instruction.
        switch (operator_type) {
            .Bang => try self.emitOp(.Not),
            .Minus => try self.emitOp(.Negate),
            else => unreachable,
        }
    }

    fn prefix(self: *Parser, tokenType: TokenType, can_assign: bool) !void {
        switch (tokenType) {
            .LeftParen => try self.grouping(),
            .Minus, .Bang => try self.unary(),
            .Identifier => try self.variable(can_assign),
            .String => try self.string(),
            .Number => try self.number(),
            .False, .True, .Nil => try self.literal(),
            .Super => try self.super(),
            .This => try self.this(),
            else => try self.prefixError(),
        }
    }

    fn prefixError(self: *Parser) !void {
        try self.errorAtPrevious("Expect expression.", .{});
    }

    fn infix(self: *Parser, tokenType: TokenType, can_assign: bool) !void {
        switch (tokenType) {
            .LeftParen => try self.call(),
            .Or => try self.or_(),
            .And => try self.and_(),
            .Dot => try self.dot(can_assign),
            .Minus, .Plus, .Slash, .Star, .BangEqual, .EqualEqual, .Greater, .GreaterEqual, .Less, .LessEqual => try self.binary(),
            else => try self.infixError(),
        }
    }

    fn infixError(self: *Parser) !void {
        try self.errorAtPrevious("Expect expression.", .{});
    }

    fn parsePrecedence(self: *Parser, precedence: Precedence) CompilerError!void {
        try self.advance();

        const can_assign = @enumToInt(precedence) <= @enumToInt(Precedence.Assignment);
        try self.prefix(self.previous.tokenType, can_assign);

        while (@enumToInt(precedence) <= @enumToInt(getPrecedence(self.current.tokenType))) {
            try self.advance();
            try self.infix(self.previous.tokenType, can_assign);
        }

        if (can_assign and try self.match(.Equal)) {
            try self.errorAtPrevious("Invalid assignment target.", .{});
        }
    }

    fn parseVariable(self: *Parser, comptime errMsg: []const u8) !u8 {
        try self.consume(.Identifier, errMsg);

        try self.declareVariable();
        if (self.compiler.scope_depth > 0) return 0;

        return try self.identifierConstant(self.previous.lexeme);
    }

    fn markInitialized(self: *Parser) void {
        if (self.compiler.scope_depth == 0) return;

        self.compiler.locals[self.compiler.local_count - 1].depth = self.compiler.scope_depth;
    }

    fn defineVariable(self: *Parser, global: u8) !void {
        if (self.compiler.scope_depth > 0) {
            self.markInitialized();
            return;
        }

        try self.emitUnaryOp(.DefineGlobal, global);
    }

    fn argumentList(self: *Parser) !u8 {
        var arg_count: u8 = 0;
        if (!self.check(.RightParen)) {
            while (true) {
                try self.expression();

                if (arg_count == Function.MAX_ARITY) {
                    try self.errorAtPrevious("Can't have more than {d} arguments.", .{ Function.MAX_ARITY });
                } else {
                    arg_count += 1;
                }

                if (!try self.match(.Comma)) break;
            }
        }

        try self.consume(.RightParen, "Expect ')' after arguments.");

        return arg_count;
    }

    fn and_(self: *Parser) !void {
        const end_jump = try self.emitJump(.JumpIfFalse);

        try self.emitOp(.Pop);
        try self.parsePrecedence(.And);

        try self.patchJump(end_jump);
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

    fn function_(self: *Parser, function_type: FunctionType) !void {
        var compiler = try Compiler.create(self.vm, self.compiler, function_type);
        self.compiler = &compiler;
        self.compiler.function.name = try Obj.String.copy(self.vm, self.previous.lexeme);
        self.beginScope();

        try self.consume(.LeftParen, "Expect '(' after function name.");
        if (!self.check(.RightParen)) {
            while (true) {
                if (self.compiler.function.arity == Function.MAX_ARITY) {
                    try self.errorAtCurrent("Can't have more than {d} parameters.", .{ Function.MAX_ARITY });
                }
                self.compiler.function.arity += 1;
                const constant = try self.parseVariable("Expect parameter name.");
                try self.defineVariable(constant);
                // Want to keep parsing variables until we reach something that isn't a comma.
                if (!try self.match(.Comma)) break;
            }
        }
        try self.consume(.RightParen, "Expect ')' after parameters.");
        try self.consume(.LeftBrace, "Expect '{' before function body.");
        try self.block();

        const function = try self.end();
        try self.emitUnaryOp(.Closure, try self.makeConstant(Value.fromObj(&function.obj)));

        var i: usize = 0;
        while (i < function.upvalueCount) : (i += 1) {
            try self.emitByte(@boolToInt(compiler.upvalues[i].is_local));
            try self.emitByte(compiler.upvalues[i].index);
        }
    }

    fn method(self: *Parser) !void {
        try self.consume(.Identifier, "Expect method name.");
        const constant = try self.identifierConstant(self.previous.lexeme);

        var function_type: FunctionType = undefined;
        if (std.mem.eql(u8, self.previous.lexeme, "init")) {
            function_type = .Initialiser;
        } else {
            function_type = .Method;
        }
        // TODO: for some reason the below causes a compiler error, not sure why.
        // const function_type = if (std.mem.eql(u8, self.previous.lexeme, "init")) .Initialiser else .Method;
        try self.function_(function_type);

        try self.emitUnaryOp(.Method, constant);
    }

    fn classDeclaration(self: *Parser) !void {
        try self.consume(.Identifier, "Expect class name.");
        const class_name = self.previous.lexeme;
        const name_constant = try self.identifierConstant(self.previous.lexeme);
        try self.declareVariable();

        try self.emitBytes(@enumToInt(OpCode.Class), name_constant);
        try self.defineVariable(name_constant);

        var class_compiler: ClassCompiler = .{ .enclosing = self.current_class };
        self.current_class = &class_compiler;

        if (try self.match(.Less)) {
            // Deal with class inheritance
            try self.consume(.Identifier, "Expect superclass name.");
            try self.variable(false);

            if (std.mem.eql(u8, class_name, self.previous.lexeme)) {
                try self.errorAtPrevious("A class can't inherit from itself.", .{});
            }

            self.beginScope();
            try self.addLocal("super");
            try self.defineVariable(0);

            try self.namedVariable(class_name, false);
            try self.emitOp(.Inherit);
            self.current_class.?.has_superclass = true;
        }

        try self.namedVariable(class_name, false);
        try self.consume(.LeftBrace, "Expect '{' before class body.");
        while (!self.check(.RightBrace) and !self.check(.EOF)) {
            try self.method();
        }
        try self.consume(.RightBrace, "Expect '}' after class body.");
        // Once we've reached the end of the methods, we no longer need the
        // class and so we can pop it off the stack.
        try self.emitOp(.Pop);

        if (self.current_class.?.has_superclass) {
            try self.endScope();
        }

        self.current_class = self.current_class.?.enclosing;
    }

    fn fnDeclaration(self: *Parser) !void {
        const global = try self.parseVariable("Expect function name.");
        self.markInitialized();
        try self.function_(.Function);
        try self.defineVariable(global);
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

        var loop_start = self.currentChunk().code.items.len;
        var maybe_exit_jump: ?usize = null;
        if (!try self.match(.Semicolon)) {
            try self.expression();
            try self.consume(.Semicolon, "Expect ';' after loop condition.");

            // Jump out of the loop if the condition is false.
            maybe_exit_jump = @intCast(usize, try self.emitJump(.JumpIfFalse));
            try self.emitOp(.Pop);
        }

        if (!try self.match(.RightParen)) {
            const bodyJump = try self.emitJump(.Jump);
            const incrementStart = self.currentChunk().code.items.len;
            try self.expression();
            try self.emitOp(.Pop);
            try self.consume(.RightParen, "Expect ')' after for clauses.");

            try self.emitLoop(loop_start);
            loop_start = incrementStart;
            try self.patchJump(bodyJump);
        }

        try self.statement();
        try self.emitLoop(loop_start);

        if (maybe_exit_jump) |exitJump| {
            try self.patchJump(exitJump);
            try self.emitOp(.Pop);
        }

        try self.endScope();
    }

    fn ifStatement(self: *Parser) !void {
        try self.consume(.LeftParen, "Expect '(' after 'if'.");
        try self.expression();
        try self.consume(.RightParen, "Expect ')' after condition.");

        const then_jump = try self.emitJump(.JumpIfFalse);
        try self.emitOp(.Pop);
        try self.statement();

        const else_jump = try self.emitJump(.Jump);

        try self.patchJump(then_jump);
        try self.emitOp(.Pop);

        if (try self.match(.Else)) try self.statement();

        try self.patchJump(else_jump);
    }

    fn printStatement(self: *Parser) !void {
        try self.expression();
        try self.consume(.Semicolon, "Expect ';' after value.");
        try self.emitOp(.Print);
    }

    fn returnStatement(self: *Parser) !void {
        if (self.compiler.function_type == .Script) {
            try self.errorAtPrevious("Can't return from top-level code.", .{});
        }

        if (try self.match(.Semicolon)) {
            try self.emitReturn();
        } else {
            // Note that even if an error is reported we still want to
            // continue with compilation to avoid a more errors since there
            // will be a trailing expression after the return statement.
            if (self.compiler.function_type == .Initialiser) {
                try self.errorAtPrevious("Can't return a value from an initializer.", .{});
            }

            try self.expression();
            try self.consume(.Semicolon, "Expect ';' after value.");
            try self.emitOp(.Return);
        }
    }

    fn whileStatement(self: *Parser) !void {
        const loop_start = self.currentChunk().code.items.len;
        try self.consume(.LeftParen, "Expect '(' after 'while'.");
        try self.expression();
        try self.consume(.RightParen, "Expect ')' after condition.");

        const exitJump = try self.emitJump(.JumpIfFalse);
        try self.emitOp(.Pop);
        try self.statement();
        try self.emitLoop(loop_start);

        try self.patchJump(exitJump);
        try self.emitOp(.Pop);
    }

    fn synchronize(self: *Parser) !void {
        self.panic_mode = false;

        while (self.current.tokenType != .EOF) {
            if (self.previous.tokenType == .Semicolon) return;

            switch (self.current.tokenType) {
                .Class, .Fn, .Var, .For, .If, .While, .Print, .Return => return,
                else => try self.advance(),
            }
        }
    }

    fn declaration(self: *Parser) CompilerError!void {
        if (try self.match(.Class)) {
            try self.classDeclaration();
        } else if (try self.match(.Fn)) {
            try self.fnDeclaration();
        } else if (try self.match(.Var)) {
            try self.varDeclaration();
        } else {
            try self.statement();
        }

        if (self.panic_mode) try self.synchronize();
    }

    fn statement(self: *Parser) CompilerError!void {
        if (try self.match(.Print)) {
            try self.printStatement();
        } else if (try self.match(.For)) {
            try self.forStatement();
        } else if (try self.match(.If)) {
            try self.ifStatement();
        } else if (try self.match(.Return)) {
            try self.returnStatement();
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
