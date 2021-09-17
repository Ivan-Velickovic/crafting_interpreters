const std = @import("std");
const buildOptions = @import("build_options");
const compiler = @import("compiler.zig");
const debug = @import("debug.zig");
const main = @import("main.zig");
const Compiler = @import("compiler.zig").Compiler;
const Table = @import("table.zig").Table;
const Value = @import("value.zig").Value;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Obj = @import("object.zig").Obj;
const String = @import("object.zig").Obj.String;
const FixedCapacityStack = @import("stack.zig").FixedCapacityStack;
const Allocator = std.mem.Allocator;
const stdout = main.stdout;
const stderr = main.stderr;

pub const InterpretError = error{
    CompileError,
    RuntimeError,
};

pub const CallFrame = struct {
    closure: *Obj.Closure,
    ip: usize,
    start: usize, // Index into the VM stack where values for this CallFrame start.
};

pub const VM = struct {
    const FRAMES_MAX = 64;
    const STACK_MAX = FRAMES_MAX * (Compiler.MAX_LOCALS + 1);

    allocator: *Allocator,
    frames: [FRAMES_MAX]CallFrame,
    frameCount: u7,
    stack: FixedCapacityStack(Value),
    globals: Table,
    strings: Table,
    openUpvalues: ?*Obj.Upvalue,
    objects: ?*Obj,

    pub fn create(allocator: *Allocator) !VM {
        var vm = VM{
            .allocator = allocator,
            .frames = undefined,
            .frameCount = 0,
            .stack = try FixedCapacityStack(Value).create(allocator, STACK_MAX),
            .globals = Table.create(allocator),
            .strings = Table.create(allocator),
            .openUpvalues = null,
            .objects = null,
        };

        try vm.defineNative("clock", clockNative);

        return vm;
    }

    pub fn destroy(self: *VM) void {
        self.stack.destroy();
        self.globals.destroy();
        self.strings.destroy();
        self.freeObjects();
    }

    fn freeObjects(self: *VM) void {
        var object = self.objects;
        while (object) |obj| {
            const next = obj.next;
            obj.destroy(self);
            object = next;
        }
    }

    pub fn interpret(self: *VM, source: []const u8) !void {
        const function = try compiler.compile(source, self);

        self.stack.push(Value.fromObj(&function.obj));
        const closure = try Obj.Closure.create(self, function);
        // Pop the function that compile returns and push the closure that wraps it.
        _ = self.stack.pop();
        self.stack.push(Value.fromObj(&closure.obj));
        try self.call(closure, 0);

        try self.run();
    }

    fn runtimeError(self: *VM, comptime fmt: []const u8, args: anytype) !void {
        try stderr.print(fmt ++ "\n", args);

        var i: u8 = 0;
        while (i < self.frameCount) : (i += 1) {
            const frame = &self.frames[self.frameCount - 1 - i];

            // frame.ip - 1 because we want to refer to the previous instruction as
            // that is where the error occurred.
            const line = frame.closure.function.chunk.lines.items[frame.ip - 1];
            try stderr.print("[line {}] in ", .{line});

            if (frame.closure.function.name) |name| {
                try stderr.print("{s}()\n", .{name.chars});
            } else {
                try stderr.print("script\n", .{});
            }
        }

        self.resetStack();
    }

    fn defineNative(self: *VM, name: []const u8, function: Obj.Native.Fn) !void {
        const nameObj = &(try Obj.String.copy(self, name)).obj;
        self.stack.push(Value.fromObj(nameObj));
        const functionObj = &(try Obj.Native.create(self, function)).obj;
        self.stack.push(Value.fromObj(functionObj));

        _ = try self.globals.set(self.stack.items[0].Obj.asType(Obj.String), self.stack.items[1]);
        _ = self.stack.pop();
        _ = self.stack.pop();
    }

    fn resetStack(self: *VM) void {
        self.stack.resize(0);
        self.frameCount = 0;
        self.openUpvalues = null;
    }

    fn peek(self: *VM, distance: usize) Value {
        return self.stack.items[self.stack.items.len - distance - 1];
    }

    fn call(self: *VM, closure: *Obj.Closure, argCount: u8) !void {
        if (argCount != closure.function.arity) {
            try self.runtimeError("Expected {d} arguments but got {d}.", .{closure.function.arity, argCount});
            return InterpretError.RuntimeError;
        }

        if (self.frameCount == FRAMES_MAX) {
            try self.runtimeError("Stack overflow.", .{});
            return InterpretError.RuntimeError;
        }

        self.frames[self.frameCount] = CallFrame{
            .closure = closure,
            .ip = 0,
            // We want the slots to start from the beggining of the bytecode
            // for a function call, so we start one before its arguments since
            // that also includes the name of the function.
            .start = self.stack.items.len - argCount - 1,
        };
        self.frameCount += 1;
    }

    fn callValue(self: *VM, callee: Value, argCount: u8) !void {
        if (callee == .Obj) {
            switch (callee.Obj.objType) {
                .Closure => return try self.call(callee.Obj.asType(Obj.Closure), argCount),
                .Native => {
                    const native = callee.Obj.asType(Obj.Native).function;
                    const result = native(argCount, self.stack.items[self.stack.items.len - argCount..]);

                    self.stack.resize(self.stack.items.len - argCount + 1);
                    self.stack.push(result);

                    return;
                },
                else => {},
            }
        }

        try self.runtimeError("Can only call functions and classes.", .{});

        return InterpretError.RuntimeError;
    }

    fn captureUpvalue(self: *VM, local: *Value) !*Obj.Upvalue {
        var prevUpvalue: ?*Obj.Upvalue = null;
        var currUpvalue = self.openUpvalues;
        while (currUpvalue) |upvalue| {
            // Since open upvalues are ordered, we iterate past upvalues, starting from the
            // top of the stack, that point to Values above the one we are looking for.
            if (@ptrToInt(upvalue.location) <= @ptrToInt(local)) break;

            prevUpvalue = upvalue;
            currUpvalue = upvalue.next;
        }

        if (currUpvalue != null and currUpvalue.?.location == local) return currUpvalue.?;

        var upvalue = try Obj.Upvalue.create(self, local);
        // If we got to here, it means that local is further up the stack than where upvalue
        // points to, so we want to insert the newly created upvalue before currUpvalue.
        upvalue.next = currUpvalue;

        if (prevUpvalue) |prev| {
            prev.next = upvalue;
        } else {
            self.openUpvalues = upvalue;
        }

        return upvalue;
    }

    fn closeUpvalues(self: *VM, last: *Value) void {
        while (self.openUpvalues) |upvalue| {
            // We want to close every open upvalue that is above the given slot in the stack.
            if (@ptrToInt(upvalue.location) < @ptrToInt(last)) break;
            // We move the Value at location from the stack to the heap allocated Upvalue,
            // we then change location to point to this "closed" value so that references
            // to the location of an upvalue are still correct.
            upvalue.closed = upvalue.location.*;
            upvalue.location = &upvalue.closed;
            self.openUpvalues = upvalue.next;
        }
    }

    fn readString(frame: *CallFrame) *Obj.String {
        return readConstant(frame).Obj.asType(Obj.String);
    }

    fn readConstant(frame: *CallFrame) Value {
        return frame.closure.function.chunk.constants.items[readByte(frame)];
    }

    fn readByte(frame: *CallFrame) u8 {
        defer frame.ip += 1;

        return frame.closure.function.chunk.code.items[frame.ip];
    }

    fn readShort(frame: *CallFrame) u16 {
        frame.ip += 2;
        const hi = @as(u16, frame.closure.function.chunk.code.items[frame.ip - 2]);
        const lo = frame.closure.function.chunk.code.items[frame.ip - 1];

        return (hi << 8) | lo;
    }

    fn binaryBooleanOp(self: *VM, op: OpCode) !void {
        const rhs = self.stack.pop();
        const lhs = self.stack.pop();

        if (lhs != .Number or rhs != .Number) {
            try self.runtimeError("Operands must be numbers.", .{});
            return InterpretError.RuntimeError;
        }

        const result = switch (op) {
            .Greater => lhs.Number > rhs.Number,
            .Less => lhs.Number < rhs.Number,
            else => unreachable
        };

        self.stack.push(Value.fromBool(result));
    }

    fn binaryNumericOp(self: *VM, op: OpCode) !void {
        const rhs = self.stack.pop();
        const lhs = self.stack.pop();

        if (lhs != .Number or rhs != .Number) {
            try self.runtimeError("Operands must be numbers.", .{});
            return InterpretError.RuntimeError;
        }

        const result = switch (op) {
            .Subtract => lhs.Number - rhs.Number,
            .Multiply => lhs.Number * rhs.Number,
            .Divide => lhs.Number / rhs.Number,
            else => unreachable
        };

        self.stack.push(Value.fromNumber(result));
    }

    fn concatenate(self: *VM, a: *String, b: *String) !void {
        const slices = [_][]const u8 {a.chars, b.chars};
        const concatenatedChars = try std.mem.concat(self.allocator, u8, &slices);

        const result = try String.take(self, concatenatedChars);
        self.stack.push(Value.fromObj(&result.obj));
    }

    pub fn run(self: *VM) !void {
        var frame = &self.frames[self.frameCount - 1];

        while (true) {
            if (buildOptions.debugTraceExecution) {
                try stdout.print("          ", .{});
                for (self.stack.items) |value| {
                    try stdout.print("[ {} ]", .{value});
                }

                try stdout.print("\n", .{});
                _ = try debug.disassembleInstruction(&frame.closure.function.chunk, frame.ip);
            }

            const opCode = @intToEnum(OpCode, readByte(frame));
            switch (opCode) {
                .Constant => self.stack.push(readConstant(frame)),
                .Nil => self.stack.push(Value.nil()),
                .True => self.stack.push(Value.fromBool(true)),
                .False => self.stack.push(Value.fromBool(false)),
                .Pop => _ = self.stack.pop(),
                .GetLocal => {
                    const slot = readByte(frame);
                    self.stack.push(self.stack.items[frame.start + slot]);
                },
                .SetLocal => {
                    const slot = readByte(frame);
                    self.stack.items[frame.start + slot] = self.peek(0);
                },
                .GetGlobal => {
                    const name = readString(frame);
                    if (self.globals.get(name)) |value| {
                        self.stack.push(value.*);
                    } else {
                        try self.runtimeError("Undefined variable '{s}'.", .{name.chars});
                        return InterpretError.RuntimeError;
                    }
                },
                .DefineGlobal => {
                    const name = readString(frame);
                    _ = try self.globals.set(name, self.peek(0));
                    _ = self.stack.pop();
                },
                .SetGlobal => {
                    const name = readString(frame);
                    if (try self.globals.set(name, self.peek(0))) {
                        _ = self.globals.delete(name);
                        try self.runtimeError("Undefined variable '{s}'.", .{name.chars});
                        return InterpretError.RuntimeError;
                    }
                },
                .GetUpvalue => {
                    const slot = readByte(frame);
                    self.stack.push(frame.closure.upvalues.items[slot].location.*);
                },
                .SetUpvalue => {
                    const slot = readByte(frame);
                    frame.closure.upvalues.items[slot].location.* = self.peek(0);
                },
                .Equal => {
                    const rhs = self.stack.pop();
                    const lhs = self.stack.pop();
                    self.stack.push(Value.fromBool(Value.isEqual(lhs, rhs)));
                },
                .Greater, .Less => try self.binaryBooleanOp(opCode),
                .Subtract, .Multiply, .Divide => try self.binaryNumericOp(opCode),
                .Add => {
                    const rhs = self.stack.pop();
                    const lhs = self.stack.pop();

                    if (Obj.isType(lhs, .String) and Obj.isType(rhs, .String)) {
                        try self.concatenate(lhs.Obj.asType(String), rhs.Obj.asType(String));
                    } else if (lhs == .Number and rhs == .Number) {
                        self.stack.push(Value.fromNumber(lhs.Number + rhs.Number));
                    } else {
                        try self.runtimeError("Operands must be two numbers or two strings.", .{});
                        return InterpretError.RuntimeError;
                    }
                },
                .Not => {
                    const value = self.stack.pop();
                    self.stack.push(Value.fromBool(value.isFalsey()));
                },
                .Negate => {
                    const value = self.stack.pop();
                    if (value != .Number) {
                        try self.runtimeError("Operand must be a number.", .{});
                        return InterpretError.RuntimeError;
                    }

                    self.stack.push(Value.fromNumber(-value.Number));
                },
                .Print => try stdout.print("{}\n", .{self.stack.pop()}),
                .Jump => {
                    const offset = readShort(frame);
                    frame.ip += offset;
                },
                .JumpIfFalse => {
                    const offset = readShort(frame);
                    if (self.peek(0).isFalsey()) frame.ip += offset;
                },
                .Loop => {
                    const offset = readShort(frame);
                    frame.ip -= offset;
                },
                .Call => {
                    const argCount = readByte(frame);
                    try self.callValue(self.peek(argCount), argCount);
                    // Since calling a function will create a new CallFrame on the frames stack,
                    // so we update the frame that is being interpreted.
                    frame = &self.frames[self.frameCount - 1];
                },
                .Closure => {
                    const function = readConstant(frame).Obj.asType(Obj.Function);
                    const closure = try Obj.Closure.create(self, function);
                    self.stack.push(Value.fromObj(&closure.obj));

                    var i: u9 = 0;
                    while (i < function.upvalueCount) : (i += 1) {
                        const isLocal = readByte(frame) == 1;
                        const index = readByte(frame);
                        if (isLocal) {
                            const upvalue = try self.captureUpvalue(&self.stack.items[frame.start + index]);
                            try closure.upvalues.append(upvalue);
                        } else {
                            try closure.upvalues.append(frame.closure.upvalues.items[index]);
                        }
                    }
                },
                .CloseUpvalue => {
                    self.closeUpvalues(&self.stack.items[self.stack.items.len - 1]);
                    _ = self.stack.pop();
                },
                .Return => {
                    const result = self.stack.pop();
                    self.closeUpvalues(&self.stack.items[frame.start]);
                    self.frameCount -= 1;
                    if (self.frameCount == 0) {
                        // pop the top-level "main" function if this was the last CallFrame.
                        _ = self.stack.pop();
                        return;
                    }

                    // Since we want to get rid of everything to do with this CallFrame that
                    // is being discarded, we resize the stack such that it now ends at the
                    // element before this CallFrame started.
                    self.stack.resize(frame.start);

                    self.stack.push(result);
                    frame = &self.frames[self.frameCount - 1];
                },
            }
        }
    }
};

// Built-in functions for Lox programs.

// `clock()` - Timestamp in seconds, relative to UTC 1970-01-01 (as specified in Zig's stdlib).
fn clockNative(_: u8, _: []Value) Value {
    return Value.fromNumber(@intToFloat(f64, std.time.timestamp()));
}
