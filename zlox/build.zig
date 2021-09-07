const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    var debugAll = b.option(bool, "debug-all", "Run with all debug options set") orelse false;
    var debugPrintCode = b.option(bool, "debug-print-code", "Print disassembled code") orelse false;
    var debugTraceExecution = b.option(bool, "debug-trace-exec", "Print execution trace") orelse false;
    var debugDetectMemLeaks = b.option(bool, "debug-detect-mem-leaks", "Detect memory leaks") orelse false;

    const exe = b.addExecutable("zlox", "src/main.zig");

    exe.addBuildOption(bool, "debugAll", debugAll);

    // If the general debugAll option has been set then we set all debug
    // sub-options to true.
    if (debugAll) {
        debugPrintCode = true;
        debugTraceExecution = true;
        debugDetectMemLeaks = true;
    }

    exe.addBuildOption(bool, "debugPrintCode", debugPrintCode);
    exe.addBuildOption(bool, "debugTraceExecution", debugTraceExecution);
    exe.addBuildOption(bool, "debugDetectMemLeaks", debugDetectMemLeaks);

    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
