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

    var all = b.option(bool, "debug-all", "Run with all debug options set") orelse false;
    var printCode = b.option(bool, "debug-print-code", "Print disassembled code") orelse false;
    var traceExecution = b.option(bool, "debug-trace-exec", "Print execution trace") orelse false;
    var detectMemLeaks = b.option(bool, "debug-detect-mem-leaks", "Detect internal compiler memory leaks") orelse false;
    var logGC = b.option(bool, "debug-log-gc", "Log information when the GC is invoked") orelse false;
    var stressGC = b.option(bool, "debug-stress-gc", "Invoke GC as often as possible") orelse false;

    const exe = b.addExecutable("zlox", "src/main.zig");
    exe.setTarget(target);
    exe.setBuildMode(mode);

    const debugOptions = b.addOptions();
    exe.addOptions("debug_options", debugOptions);

    debugOptions.addOption(bool, "all", all);
    // If the "all" option has been set then we set all debug sub-options to true.
    if (all) {
        printCode = true;
        traceExecution = true;
        detectMemLeaks = true;
        logGC = true;
        stressGC = true;
    }
    debugOptions.addOption(bool, "printCode", printCode);
    debugOptions.addOption(bool, "traceExecution", traceExecution);
    debugOptions.addOption(bool, "detectMemLeaks", detectMemLeaks);
    debugOptions.addOption(bool, "logGC", logGC);
    debugOptions.addOption(bool, "stressGC", stressGC);

    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
