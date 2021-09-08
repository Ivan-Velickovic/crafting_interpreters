const std = @import("std");
const buildOptions = @import("build_options");
const compiler = @import("compiler.zig");
const debug = @import("debug.zig");
const main = @import("main.zig");
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

pub const VM = struct {
    const STACK_MAX = 256;

    allocator: *Allocator,
    chunk: Chunk,
    instructionIndex: usize,
    stack: FixedCapacityStack(Value),
    globals: Table,
    strings: Table,
    objects: ?*Obj,

    pub fn create(allocator: *Allocator) !VM {
        var vm = VM{
            .allocator = allocator,
            .chunk = Chunk.create(allocator),
            .instructionIndex = 0,
            .globals = Table.create(allocator),
            .strings = Table.create(allocator),
            .stack = try FixedCapacityStack(Value).create(allocator, STACK_MAX),
            .objects = null,
        };

        return vm;
    }

    pub fn destroy(self: *VM) void {
        self.chunk.destroy();
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
        try compiler.compile(self, source);

        try self.run();
    }

    fn runtimeError(self: *VM, comptime fmt: []const u8, args: anytype) !void {
        try stderr.print(fmt ++ "\n", args);

        std.debug.assert(self.instructionIndex != 0);

        const line = self.chunk.lines.items[self.instructionIndex - 1];
        try stderr.print("[line {}] in script\n", .{line});

        self.resetStack();
    }

    fn resetStack(self: *VM) void {
        self.stack.resize(0);
    }

    fn peek(self: *VM, distance: usize) Value {
        return self.stack.items[self.stack.items.len - distance - 1];
    }

    fn readString(self: *VM) *Obj.String {
        return self.readConstant().Obj.asType(Obj.String);
    }

    fn readConstant(self: *VM) Value {
        return self.chunk.constants.items[self.readByte()];
    }

    fn readByte(self: *VM) u8 {
        defer self.instructionIndex += 1;

        return self.chunk.code.items[self.instructionIndex];
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

        const result = try String.take(concatenatedChars, self);
        self.stack.push(Value.fromObj(&result.obj));
    }

    pub fn run(self: *VM) !void {
        while (true) {
            if (buildOptions.debugTraceExecution) {
                try stdout.print("          ", .{});
                for (self.stack.items) |value| {
                    try stdout.print("[ {} ]", .{value});
                }

                try stdout.print("\n", .{});
                _ = try debug.disassembleInstruction(&self.chunk, self.instructionIndex);
            }

            const opCode = @intToEnum(OpCode, self.readByte());
            switch (opCode) {
                .Constant => self.stack.push(self.readConstant()),
                .Nil => self.stack.push(Value.nil()),
                .True => self.stack.push(Value.fromBool(true)),
                .False => self.stack.push(Value.fromBool(false)),
                .Pop => _ = self.stack.pop(),
                .GetLocal => {
                    const slot = self.readByte();
                    self.stack.push(self.stack.items[slot]);
                },
                .SetLocal => {
                    const slot = self.readByte();
                    self.stack.items[slot] = self.peek(0);
                },
                .GetGlobal => {
                    const name = self.readString();
                    if (self.globals.get(name)) |value| {
                        self.stack.push(value.*);
                    } else {
                        try self.runtimeError("Undefined variable '{s}'.", .{name.chars});
                        return InterpretError.RuntimeError;
                    }
                },
                .DefineGlobal => {
                    const name = self.readString();
                    _ = try self.globals.set(name, self.peek(0));
                    _ = self.stack.pop();
                },
                .SetGlobal => {
                    const name = self.readString();
                    if (try self.globals.set(name, self.peek(0))) {
                        _ = self.globals.delete(name);
                        try self.runtimeError("Undefined variable '{s}'.", .{name.chars});
                        return InterpretError.RuntimeError;
                    }
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
                // For Return, we simply want to exit the interpreter.
                .Return => return,
            }
        }
    }
};
