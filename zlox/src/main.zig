const std = @import("std");
const builtin = @import("builtin");
const debug_options = @import("debug_options");
const GC = @import("memory.zig").GC;
const VM = @import("vm.zig").VM;
const InterpretError = @import("vm.zig").InterpretError;
const scanner = @import("scanner.zig");
const process = std.process;
const Allocator = std.mem.Allocator;

pub const stdout = std.io.getStdOut().writer();
pub const stderr = std.io.getStdErr().writer();

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const gpa_alloc = gpa.allocator();

// nocheckin: CTRL+D on repl causes error.
// nocheckin: convention is that varibales have _ instead of camel case.
// nocheckin: figure out this init vs create shit once and for all.
// nocheckin: figure out whether or not debug messages should go to stderr instead of stdout? eg it makes sense to use std.log
// to log stuff for GC.
pub fn main() !void {
    if (debug_options.printCode)        std.log.info("[debug option set] Print code", .{});
    if (debug_options.traceExecution)   std.log.info("[debug option set] Trace execution", .{});
    if (debug_options.detectMemLeaks)   std.log.info("[debug option set] Detect compiler memory leaks option set", .{});
    if (debug_options.logGC)            std.log.info("[debug option set] Log garbage collections", .{});
    if (debug_options.stressGC)         std.log.info("[debug option set] Invoke GC as much as possible", .{});
    if (debug_options.all)              std.log.info("Running with all debug options set.\n", .{});

    // nocheckin: come back to this, recheck all the allocators and how they're used in this file
    // We don't do anything with whether or not there were leaks since
    // something would have been printed by the deinit funciton anyways.
    // defer _ = gpa.detectLeaks();
    defer _ = gpa.deinit();

    // nocheckin: explain vm create vs init
    var vm = VM.create();
    defer vm.destroy();

    // if (debug_options.detectMemLeaks) _ = gpa.detectLeaks();

    var gc: GC = undefined;
    GC.init(&gc, &vm, gpa_alloc);

    try vm.init(&gc);

    const args = try process.argsAlloc(gpa_alloc);
    defer process.argsFree(gpa_alloc, args);

    switch (args.len) {
        1 => try repl(&vm),
        2 => try runFile(&vm, args[1]),
        else => {
            std.debug.print("Usage: lox [path]\n", .{});
            process.exit(64); // command-line usage error code nocheckin: comment all the other codes cos idk what the fuck they mean.
        },
    }
}

fn runFile(vm: *VM, path: []const u8) !void {
    const source = try readFile(gpa_alloc, path);
    defer gpa_alloc.free(source);

    vm.interpret(source) catch |err| {
        if (err == InterpretError.CompileError) std.process.exit(65);
        if (err == InterpretError.RuntimeError) std.process.exit(70);
        return err;
    };
}

fn readFile(allocator: Allocator, path: []const u8) ![]const u8 {
    // By default the mode that openFile will open the file in is read only.
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        try stderr.print("Could not open file for reading: {}\n", .{ err });
        std.process.exit(74);
    };
    defer file.close();

    const fileSize = (try file.stat()).size;

    const buffer = try allocator.alloc(u8, fileSize);
    const bytesRead = try file.read(buffer);

    if (bytesRead != fileSize) {
        try stderr.print("Could not read all of the file.\n", .{});
        std.process.exit(74);
    }

    return buffer;
}

fn repl(vm: *VM) !void {
    var line: [1024]u8 = undefined;
    while (true) {
        try stdout.print("> ", .{});

        const input = (try nextLine(std.io.getStdIn().reader(), &line)).?;
        vm.interpret(input) catch |err| {
            if (err == InterpretError.CompileError or err == InterpretError.RuntimeError) continue;
            return err;
        };
    }
}

fn nextLine(reader: anytype, buffer: []u8) !?[]const u8 {
    var line = (try reader.readUntilDelimiterOrEof(
        buffer,
        '\n',
    )) orelse return null;

    if (builtin.os.tag == .windows) {
        line = std.mem.trimRight(u8, line, "\r");
    }

    return line;
}
