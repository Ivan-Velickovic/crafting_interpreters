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
        .Return =>          simpleInstruction("OP_RETURN", offset),
    };
}
