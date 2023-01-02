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
    var debug_prints = b.option(bool, "debug-prints", "printf debugging will be shown") orelse false;
    var print_code = b.option(bool, "debug-print-code", "Print disassembled code") orelse false;
    var trace_execution = b.option(bool, "debug-trace-exec", "Print execution trace") orelse false;
    var detect_mem_leaks = b.option(bool, "debug-detect-mem-leaks", "Detect internal compiler memory leaks") orelse false;
    var log_gc = b.option(bool, "debug-log-gc", "Log information when the GC is invoked") orelse false;
    var stress_gc = b.option(bool, "debug-stress-gc", "Invoke GC as often as possible") orelse false;
    const nan_boxing = b.option(bool, "nan-boxing", "Enable NaN-boxing optimisation in the VM") orelse false;

    const exe = b.addExecutable("zlox", "src/main.zig");
    exe.setTarget(target);
    exe.setBuildMode(mode);

    const zlox_options = b.addOptions();
    exe.addOptions("zlox_options", zlox_options);
    zlox_options.addOption(bool, "nan_boxing", nan_boxing);

    const debug_options = b.addOptions();
    exe.addOptions("debug_options", debug_options);

    debug_options.addOption(bool, "all", all);
    // If the "all" option has been set then we set all debug sub-options to true.
    if (all) {
        debug_prints = true;
        print_code = true;
        trace_execution = true;
        detect_mem_leaks = true;
        log_gc = true;
        stress_gc = true;
    }
    debug_options.addOption(bool, "debug_prints", debug_prints);
    debug_options.addOption(bool, "print_code", print_code);
    debug_options.addOption(bool, "trace_execution", trace_execution);
    debug_options.addOption(bool, "detect_mem_leaks", detect_mem_leaks);
    debug_options.addOption(bool, "log_gc", log_gc);
    debug_options.addOption(bool, "stress_gc", stress_gc);

    exe.install();
}
