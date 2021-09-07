const std = @import("std");
const buildOptions = @import("build_options");
const VM = @import("vm.zig").VM;
const InterpretError = @import("vm.zig").InterpretError;
const scanner = @import("scanner.zig");
const process = std.process;
const Allocator = std.mem.Allocator;

pub const stdout = std.io.getStdOut().writer();
pub const stderr = std.io.getStdErr().writer();

pub fn main() !void {
    if (buildOptions.debugPrintCode)       std.log.info("Print code option set.", .{});
    if (buildOptions.debugTraceExecution)  std.log.info("Trace execution option set.", .{});
    if (buildOptions.debugDetectMemLeaks)  std.log.info("Detect memory leaks option set.", .{});
    if (buildOptions.debugAll)             std.log.info("Running with all debug options set.\n", .{});

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    // We don't do anything with whether or not there were leaks since
    // something would have been printed by the deinit funciton anyways.
    defer _ = gpa.deinit();
    if (buildOptions.debugDetectMemLeaks) _ = gpa.detectLeaks();

    const args = try process.argsAlloc(&gpa.allocator);
    defer process.argsFree(&gpa.allocator, args);

    switch (args.len) {
        1 => try repl(&gpa.allocator),
        2 => try runFile(&gpa.allocator, args[1]),
        else => {
            std.debug.warn("Usage: lox [path]\n", .{});
            process.exit(64);
        },
    }
}

fn runFile(allocator: *Allocator, path: []const u8) !void {
    const source = try readFile(allocator, path);
    defer allocator.free(source);

    var vm = try VM.create(allocator);
    defer vm.destroy();

    vm.interpret(source) catch |err| {
        if (err == InterpretError.CompileError) std.c.exit(65);
        if (err == InterpretError.RuntimeError) std.c.exit(70);
        return err;
    };
}

fn readFile(allocator: *Allocator, path: []const u8) ![]const u8 {
    const file = try std.fs.cwd().openFile(path, .{ .read = true });
    defer file.close();

    const fileStat = try file.stat();
    const fileSize = fileStat.size;

    const buffer = try allocator.alloc(u8, fileSize);
    const bytesRead = try file.read(buffer);

    if (bytesRead != fileSize) {
        try stderr.print("Could not read all of the file.\n", .{});
        std.c.exit(74);
    }

    return buffer;
}

fn repl(allocator: *Allocator) !void {
    var vm = try VM.create(allocator);
    defer vm.destroy();

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

    if (std.builtin.os.tag == .windows) {
        line = std.mem.trimRight(u8, line, "\r");
    }

    return line;
}
