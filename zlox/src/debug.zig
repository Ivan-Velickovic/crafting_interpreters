const std = @import("std");
const stdout = @import("main.zig").stdout;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;

pub fn disassembleChunk(chunk: *Chunk, name: []const u8) !void {
    try stdout.print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < chunk.code.items.len) {
        offset = try disassembleInstruction(chunk, offset);
    }
}

fn constantInstruction(name: []const u8, chunk: *Chunk, offset: usize) !usize {
    const constant = chunk.code.items[offset + 1];
    const value = chunk.constants.items[constant];
    try stdout.print("{s: <16} {d: >4} '{s}'\n", .{ name, constant, value });

    return offset + 2;
}

fn simpleInstruction(name: []const u8, offset: usize) !usize {
    try stdout.print("{s}\n", .{name});

    return offset + 1;
}

fn byteInstruction(name: []const u8, chunk: *Chunk, offset: usize) !usize {
    const slot = chunk.code.items[offset + 1];
    try stdout.print("{s: <16} {d: >4}\n", .{ name, slot });

    return offset + 2;
}

fn jumpInstruction(name: []const u8, sign: i8, chunk: *Chunk, offset: usize) !usize {
    var jump = @as(u16, chunk.code.items[offset + 1]) << 8;
    jump |= chunk.code.items[offset + 2];

    // Since sign is possibly negative, we have to do these casts, since jumpEnd could be negative.
    const jumpEnd = @intCast(isize, offset) + 3 + sign * @as(i32, jump);
    try stdout.print("{s: <16} {d: >4} -> {d}\n", .{ name, offset, jumpEnd });

    return offset + 3;
}

pub fn disassembleInstruction(chunk: *Chunk, offset: usize) !usize {
    try stdout.print("{d:0>4} ", .{offset});

    if (offset > 0 and chunk.lines.items[offset] == chunk.lines.items[offset - 1]) {
        try stdout.print("   | ", .{});
    } else {
        try stdout.print("{d: >4} ", .{chunk.lines.items[offset]});
    }

    const instruction = chunk.code.items[offset];
    return switch (@intToEnum(OpCode, instruction)) {
        .Constant =>        constantInstruction("OP_CONSTANT", chunk, offset),
        .Nil =>             simpleInstruction("OP_NIL", offset),
        .True =>            simpleInstruction("OP_TRUE", offset),
        .False =>           simpleInstruction("OP_FALSE", offset),
        .Pop =>             simpleInstruction("OP_POP", offset),
        .GetLocal =>        byteInstruction("OP_GET_LOCAL", chunk, offset),
        .SetLocal =>        byteInstruction("OP_SET_LOCAL", chunk, offset),
        .GetGlobal =>       constantInstruction("OP_GET_GLOBAL", chunk, offset),
        .DefineGlobal =>    constantInstruction("OP_DEFINE_GLOBAL", chunk, offset),
        .SetGlobal =>       constantInstruction("OP_SET_GLOBAL", chunk, offset),
        .Equal =>           simpleInstruction("OP_EQUAL", offset),
        .Greater =>         simpleInstruction("OP_GREATER", offset),
        .Less =>            simpleInstruction("OP_LESS", offset),
        .Add =>             simpleInstruction("OP_ADD", offset),
        .Subtract =>        simpleInstruction("OP_SUBTRACT", offset),
        .Multiply =>        simpleInstruction("OP_MULTIPLY", offset),
        .Divide =>          simpleInstruction("OP_DIVIDE", offset),
        .Not =>             simpleInstruction("OP_NOT", offset),
        .Negate =>          simpleInstruction("OP_NEGATE", offset),
        .Print =>           simpleInstruction("OP_PRINT", offset),
        .Jump =>            jumpInstruction("OP_JUMP", 1, chunk, offset),
        .JumpIfFalse =>     jumpInstruction("OP_JUMP_IF_FALSE", 1, chunk, offset),
        .Loop =>            jumpInstruction("OP_LOOP", -1, chunk, offset),
        .Return =>          simpleInstruction("OP_RETURN", offset),
    };
}
