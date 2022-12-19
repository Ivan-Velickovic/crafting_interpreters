const std = @import("std");
const debug_options = @import("debug_options");
const Parser = @import("compiler.zig").Parser;
const Compiler = @import("compiler.zig").Compiler;
const Value = @import("value.zig").Value;
const Table = @import("table.zig").Table;
const Obj = @import("object.zig").Obj;
const VM = @import("vm.zig").VM;
const Allocator = std.mem.Allocator;

pub const GC = struct {
    const HEAP_GROW_FACTOR = 2;

    vm: *VM,
    internalAllocator: Allocator, // This is what we use to actually allocate on the heap
    // allocator: Allocator, // This is the allocator that zlox uses, memory allocated with this gets garbage collected

    pub fn init(self: *GC, vm: *VM, internalAllocator: Allocator) void {
        self.vm = vm;
        self.internalAllocator = internalAllocator;
    }

    pub fn allocator(self: *GC) Allocator {
        return .{
            .ptr = @ptrCast(*anyopaque, self),
            .vtable = &.{
                .alloc = alloc,
                .resize = resize,
                .free = free,
            },
        };
    }

    fn alloc(ctx: *anyopaque, len: usize, ptr_align: u8, ret_addr: usize) ?[*]u8 {
        const self = @ptrCast(*GC, @alignCast(@alignOf(GC), ctx));
        self.vm.bytesAllocated += len;
        if (debug_options.stressGC or self.vm.bytesAllocated > self.vm.nextGC) {
            self.collectGarbage() catch unreachable;
        }

        return self.internalAllocator.vtable.alloc(self.internalAllocator.ptr, len, ptr_align, ret_addr);
    }

    fn resize(ctx: *anyopaque, buf: []u8, buf_align: u8, new_len: usize, ret_addr: usize) bool {
        const self = @ptrCast(*GC, @alignCast(@alignOf(GC), ctx));
        if (new_len > buf.len) {
            self.vm.bytesAllocated += new_len - buf.len;
        } else {
            self.vm.bytesAllocated -= buf.len - new_len;
        }

        if ((debug_options.stressGC and new_len > buf.len) or self.vm.bytesAllocated > self.vm.nextGC) {
            self.collectGarbage() catch unreachable;
        }

        return self.internalAllocator.vtable.resize(self.internalAllocator.ptr, buf, buf_align, new_len, ret_addr);
    }

    fn free(ctx: *anyopaque, buf: []u8, buf_align: u8, ret_addr: usize) void {
        const self = @ptrCast(*GC, @alignCast(@alignOf(GC), ctx));
        if (debug_options.stressGC or self.vm.bytesAllocated > self.vm.nextGC) {
            self.collectGarbage() catch unreachable;
        }

        self.internalAllocator.vtable.free(self.internalAllocator.ptr, buf, buf_align, ret_addr);
    }

    fn collectGarbage(self: *GC) !void {
        if (debug_options.logGC) {
            // nocheckin: we can't do "try stdout.print" here since collectGarbage is used in the allocator functions and they must
            // return Allocator.Error set
            std.debug.print("-- GC begin\n", .{});
            const before = self.vm.bytesAllocated;
            defer {
                std.debug.print("-- GC end\n", .{});
                std.debug.print("   collected {d} bytes (from {d} to {d}) next at {d}\n",
                    .{ before - self.vm.bytesAllocated, before, self.vm.bytesAllocated, self.vm.nextGC });
            }
        }

        try self.markRoots();
        try self.traceReferences();
        tableRemoveWhite(&self.vm.strings);
        self.sweep();

        self.vm.nextGC = self.vm.bytesAllocated * HEAP_GROW_FACTOR;
    }

    fn markRoots(self: *GC) !void {
        for (self.vm.stack.items) |slot| {
            try self.markValue(slot);
        }

        var i: usize = 0;
        while (i < self.vm.frameCount) : (i += 1) {
            try self.markObject(&self.vm.frames[i].closure.obj);
        }

        try self.markTable(&self.vm.globals);
        try self.markCompilerRoots();
    }

    fn markCompilerRoots(self: *GC) !void {
        // nocheckin: make note that if a GC occurs after the final "end" of a compiler, it may cause a double mark?
        var currCompiler: ?*Compiler = @fieldParentPtr(Parser, "vm", &self.vm).compiler;
        while (currCompiler) |compiler| {
            try self.markObject(&compiler.function.obj);
            currCompiler = compiler.enclosing;
        }
    }

    // nocheckin: put this in Value.zig/Object.zig?
    fn markValue(self: *GC, value: Value) !void {
        if (value == .Obj) try self.markObject(value.Obj);
    }

    fn markTable(self: *GC, table: *Table) !void {
        for (table.entries) |entry| {
            if (entry.key) |key| try self.markObject(&key.obj);
            try self.markValue(entry.value);
        }
    }

    fn markValues(self: *GC, array: *std.ArrayList(Value)) !void {
        for (array.items) |value| {
            try self.markValue(value);
        }
    }

    fn markObject(self: *GC, object: *Obj) !void {
        if (object.isMarked) return;

        if (debug_options.logGC) {
            // nocheckin
            std.debug.print("address: {}\n", .{ @ptrToInt(object) });
            std.debug.print("{*} mark {s}\n", .{ object, Value.fromObj(object) });
        }

        try self.vm.grayStack.append(object);
    }

    fn blackenObject(self: *GC, object: *Obj) !void {
        if (debug_options.logGC) {
            std.debug.print("{*} blacken {s}\n", .{ object, Value.fromObj(object) });
        }

        switch (object.objType) {
            .Class => {
                const class = object.asType(Obj.Class);
                try self.markObject(&class.name.obj);
            },
            .Instance => {
                const instance = object.asType(Obj.Instance);
                try self.markObject(&instance.class.obj);
                try self.markTable(&instance.fields);
            },
            .Upvalue => try self.markValue(object.asType(Obj.Upvalue).closed),
            .Function => {
                const function = object.asType(Obj.Function);
                if (function.name) |name| try self.markObject(&name.obj);
                try self.markValues(&function.chunk.constants);
            },
            .Closure => {
                const closure = object.asType(Obj.Closure);
                try self.markObject(&closure.function.obj);

                for (closure.upvalues.items) |upvalue| {
                    try self.markObject(&upvalue.obj);
                }
            },
            else => {}
        }
    }

    fn traceReferences(self: *GC) !void {
        while (self.vm.grayStack.items.len > 0) {
            const object = self.vm.grayStack.pop();
            try self.blackenObject(object);
        }
    }

    fn sweep(self: *GC) void {
        var prev: ?*Obj = null;
        var curr = self.vm.objects;
        while (curr) |object| {
            if (object.isMarked) {
                // If the object is black and thefore reachable, we want to skip over it
                // and mark it white since all objects need to be white at the start of a GC.
                object.isMarked = false;
                prev = object;
                curr = object.next;
            } else {
                // Otherwise we want to remove the object from the linked list and free it.
                const unreached = object;
                if (prev != null) {
                    prev.?.next = object.next;
                } else {
                    self.vm.objects = object.next;
                }

                curr = object.next;
                unreached.destroy(self.vm);
            }
        }
    }

    fn tableRemoveWhite(table: *Table) void {
        for (table.entries) |entry| {
            if (entry.key) |key| {
                if (!key.obj.isMarked) _ = table.delete(key);
            }
        }
    }
};
