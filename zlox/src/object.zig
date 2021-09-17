const std = @import("std");
const Compiler = @import("Compiler.zig").Compiler;
const VM = @import("vm.zig").VM;
const Value = @import("value.zig").Value;
const Chunk = @import("chunk.zig").Chunk;

pub const Obj = struct {
    objType: Type,
    next: ?*Obj,

    const Type = enum {
        String, Function, Native, Closure, Upvalue,
    };

    fn create(vm: *VM, comptime T: type, comptime objType: Type) !*Obj {
        const ptr = try vm.allocator.create(T);

        ptr.obj = Obj{
            .objType = objType,
            .next = vm.objects,
        };

        vm.objects = &ptr.obj;

        return &ptr.obj;
    }

    pub fn destroy(self: *Obj, vm: *VM) void {
        switch (self.objType) {
            .String => self.asType(String).destroy(vm),
            .Function => self.asType(Function).destroy(vm),
            .Native => self.asType(Native).destroy(vm),
            .Closure => self.asType(Closure).destroy(vm),
            .Upvalue => self.asType(Upvalue).destroy(vm),
        }
    }

    pub fn asType(self: *Obj, comptime T: type) *T {
        return @fieldParentPtr(T, "obj", self);
    }

    pub fn isType(value: Value, objType: Type) bool {
        return value == .Obj and value.Obj.objType == objType;
    }

    pub const Upvalue = struct {
        obj: Obj,
        location: *Value,
        closed: Value,
        next: ?*Upvalue,

        pub fn create(vm: *VM, slot: *Value) !*Upvalue {
            const obj = try Obj.create(vm, Upvalue, .Upvalue);
            const upvalue = obj.asType(Upvalue);
            upvalue.* = Upvalue{
                .obj = obj.*,
                .location = slot,
                .closed = Value.nil(),
                .next = null,
            };

            return upvalue;
        }

        fn destroy(self: *Upvalue, vm: *VM) void {
            // Upvalues do not own the variable that it references, hence we only free the object.
            vm.allocator.destroy(self);
        }
    };

    pub const Closure = struct {
        obj: Obj,
        function: *Function,
        upvalues: std.ArrayList(*Upvalue),

        pub fn create(vm: *VM, function: *Function) !*Closure {
            const obj = try Obj.create(vm, Closure, .Closure);
            const closure = obj.asType(Closure);
            closure.* = Closure{
                .obj = obj.*,
                .function = function,
                .upvalues = std.ArrayList(*Upvalue).init(vm.allocator),
            };

            return closure;
        }

        fn destroy(self: *Closure, vm: *VM) void {
            // GC handles destroying function since there can be multiple references to the
            // function that the closure wraps over.
            self.upvalues.deinit();
            vm.allocator.destroy(self);
        }
    };

    pub const Native = struct {
        pub const Fn = fn (argCount: u8, args: []Value) Value;

        obj: Obj,
        function: Fn,

        pub fn create(vm: *VM, function: Fn) !*Native {
            const obj = try Obj.create(vm, Native, .Native);
            const native = obj.asType(Native);
            native.* = Native{
                .obj = obj.*,
                .function = function,
            };

            return native;
        }

        fn destroy(self: *Native, vm: *VM) void {
            vm.allocator.destroy(self);
        }
    };

    pub const Function = struct {
        pub const MAX_ARITY = std.math.maxInt(u8);

        obj: Obj,
        arity: u16, // While the max arity is the max int of u8, we still want to compile even if the source reaches this limit.
        upvalueCount: u9,
        chunk: Chunk,
        name: ?*String,

        pub fn create(vm: *VM) !*Function {
            const obj = try Obj.create(vm, Function, .Function);
            const function = obj.asType(Function);
            function.* = Function{
                .obj = obj.*,
                .arity = 0,
                .upvalueCount = 0,
                .chunk = Chunk.create(vm.allocator),
                .name = null,
            };

            return function;
        }

        fn destroy(self: *Function, vm: *VM) void {
            // Note that we let the GC handle freeing the name of the function so we don't do it here.
            self.chunk.destroy();
            vm.allocator.destroy(self);
        }
    };

    pub const String = struct {
        obj: Obj,
        chars: []const u8,
        hash: u32,

        pub fn take(vm: *VM, chars: []const u8) !*String {
            const stringHash = hash(chars);
            if (vm.strings.findString(chars, stringHash)) |interned| {
                vm.allocator.free(chars);
                return interned;
            }

            return String.create(vm, chars, stringHash);
        }

        pub fn copy(vm: *VM, chars: []const u8) !*String {
            const stringHash = hash(chars);
            if (vm.strings.findString(chars, stringHash)) |interned| {
                return interned;
            }

            const heapChars = try vm.allocator.alloc(u8, chars.len);
            std.mem.copy(u8, heapChars, chars);

            return String.create(vm, heapChars, stringHash);
        }

        fn create(vm: *VM, chars: []const u8, stringHash: u32) !*String {
            const obj = try Obj.create(vm, String, .String);
            const string = obj.asType(String);
            string.* = String{
                .obj = obj.*,
                .chars = chars,
                .hash = stringHash,
            };

            _ = try vm.strings.set(string, Value.nil());

            return string;
        }

        fn destroy(self: *String, vm: *VM) void {
            vm.allocator.free(self.chars);
            vm.allocator.destroy(self);
        }

        fn hash(chars: []const u8) u32 {
            var hashValue: u32 = 2166136261;
            var i: u32 = 0;
            while (i < chars.len) : (i += 1) {
                hashValue ^= @intCast(u8, chars[i]);
                _ = @mulWithOverflow(u32, hashValue, 16777619, &hashValue);
            }

            return hashValue;
        }
    };
};
