const std = @import("std");
const Allocator = std.mem.Allocator;
const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;

pub const OpCode = enum(u8) {
    Constant,
    Nil,
    True,
    False,
    Pop,
    GetLocal,
    GetGlobal,
    DefineGlobal,
    SetLocal,
    SetGlobal,
    GetUpvalue,
    SetUpvalue,
    GetProperty,
    SetProperty,
    Equal,
    Greater,
    Less,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,
    Print,
    Jump,
    JumpIfFalse,
    Call,
    Closure,
    Loop,
    CloseUpvalue,
    Return,
    Class,
};

pub const Chunk = struct {
    code: std.ArrayList(u8),
    lines: std.ArrayList(u32),
    constants: std.ArrayList(Value),

    pub fn create(allocator: Allocator) Chunk {
        return Chunk{
            .code = std.ArrayList(u8).init(allocator),
            .lines = std.ArrayList(u32).init(allocator),
            .constants = std.ArrayList(Value).init(allocator),
        };
    }

    pub fn destroy(self: *Chunk) void {
        self.code.deinit();
        self.lines.deinit();
        self.constants.deinit();
    }

    pub fn write(self: *Chunk, byte: u8, line: u32) !void {
        try self.code.append(byte);
        try self.lines.append(line);
    }

    pub fn addConstant(self: *Chunk, vm: *VM, value: Value) !usize {
        // We need to be careful to make the GC aware of the Value in case the
        // append causes the ArrayList to acquire more memory, hence invoking the GC.
        vm.stack.push(value);
        try self.constants.append(value);
        _ = vm.stack.pop();

        return self.constants.items.len - 1;
    }
};
