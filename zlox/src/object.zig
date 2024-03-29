const std = @import("std");
const debug_options = @import("debug_options");
const Compiler = @import("compiler.zig").Compiler;
const VM = @import("vm.zig").VM;
const Value = @import("value.zig").Value;
const Chunk = @import("chunk.zig").Chunk;
const Table = @import("table.zig").Table;

pub const Obj = struct {
    obj_type: Type,
    is_marked: bool,
    next: ?*Obj,

    const Type = enum {
        BoundMethod, Class, Closure, Function, Instance, Native, String, Upvalue
    };

    fn allocate(vm: *VM, comptime T: type, comptime obj_type: Type) !*Obj {
        const ptr = try vm.allocator.create(T);

        ptr.obj = Obj{
            .obj_type = obj_type,
            .is_marked = false,
            .next = vm.objects,
        };

        vm.objects = &ptr.obj;

        if (debug_options.log_gc) {
            std.debug.print("{*} allocate {d} for {s}\n", .{ &ptr.obj, @sizeOf(T), @tagName(obj_type) });
        }

        return &ptr.obj;
    }

    pub fn destroy(self: *Obj, vm: *VM) void {
        if (debug_options.log_gc) {
            std.debug.print("{*} free type {s}\n", .{ self, @tagName(self.obj_type) });
        }

        switch (self.obj_type) {
            .BoundMethod => self.asType(BoundMethod).destroy(vm),
            .Class => self.asType(Class).destroy(vm),
            .String => self.asType(String).destroy(vm),
            .Function => self.asType(Function).destroy(vm),
            .Native => self.asType(Native).destroy(vm),
            .Instance => self.asType(Instance).destroy(vm),
            .Closure => self.asType(Closure).destroy(vm),
            .Upvalue => self.asType(Upvalue).destroy(vm),
        }
    }

    pub fn asType(self: *Obj, comptime T: type) *T {
        return @fieldParentPtr(T, "obj", self);
    }

    pub fn isType(value: Value, obj_type: Type) bool {
        return value.isObj() and value.asObj().obj_type == obj_type;
    }

    pub const Class = struct {
        obj: Obj,
        name: *String,
        methods: Table,

        pub fn create(vm: *VM, name: *String) !*Class {
            const obj = try Obj.allocate(vm, Class, .Class);
            const class = obj.asType(Class);
            class.* = Class{
                .obj = obj.*,
                .name = name,
                .methods = Table.create(vm.allocator),
            };

            return class;
        }

        fn destroy(self: *Class, vm: *VM) void {
            self.methods.destroy();
            vm.allocator.destroy(self);
        }
    };

    pub const Instance = struct {
        obj: Obj,
        class: *Class,
        fields: Table,

        pub fn create(vm: *VM, class: *Class) !*Instance {
            const obj = try Obj.allocate(vm, Instance, .Instance);
            var instance = obj.asType(Instance);
            instance.class = class;
            instance.fields = Table.create(vm.allocator);

            return instance;
        }

        fn destroy(self: *Instance, vm: *VM) void {
            self.fields.destroy();
            vm.allocator.destroy(self);
        }
    };

    pub const BoundMethod = struct {
        obj: Obj,
        receiver: Value,
        method: *Closure,

        pub fn create(vm: *VM, receiver: Value, method: *Closure) !*BoundMethod {
            const obj = try Obj.allocate(vm, BoundMethod, .BoundMethod);
            var bound_method = obj.asType(BoundMethod);
            bound_method.receiver = receiver;
            bound_method.method = method;

            return bound_method;
        }

        fn destroy(self: *BoundMethod, vm: *VM) void {
            vm.allocator.destroy(self);
        }
    };

    pub const Upvalue = struct {
        obj: Obj,
        location: *Value,
        closed: Value,
        next: ?*Upvalue,

        pub fn create(vm: *VM, slot: *Value) !*Upvalue {
            const obj = try Obj.allocate(vm, Upvalue, .Upvalue);
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
        upvalues: std.ArrayList(*Upvalue), // TODO: explain why an arraylist is used.

        pub fn create(vm: *VM, function: *Function) !*Closure {
            const obj = try Obj.allocate(vm, Closure, .Closure);
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
        pub const Fun = *const fn (argCount: u8, args: []Value) Value;

        obj: Obj,
        function: Fun,

        pub fn create(vm: *VM, function: Fun) !*Native {
            const obj = try Obj.allocate(vm, Native, .Native);
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
        name: ?*String, // TODO: explain why this name has to be an optional

        pub fn create(vm: *VM) !*Function {
            const obj = try Obj.allocate(vm, Function, .Function);
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
            const obj = try Obj.allocate(vm, String, .String);
            const string = obj.asType(String);
            string.* = String{
                .obj = obj.*,
                .chars = chars,
                .hash = stringHash,
            };

            // Inserting into the table could trigger an allocation so we
            // push the string onto the value stack to make the GC aware of it.
            vm.stack.push(Value.fromObj(&string.obj));
            _ = try vm.strings.set(string, Value.nil());
            _ = vm.stack.pop();

            return string;
        }

        fn destroy(self: *String, vm: *VM) void {
            vm.allocator.free(self.chars);
            vm.allocator.destroy(self);
        }

        fn hash(chars: []const u8) u32 {
            var hashValue: u32 = 2166136261;
            var i: usize = 0;
            while (i < chars.len) : (i += 1) {
                hashValue ^= @intCast(u8, chars[i]);
                hashValue *%= 16777619;
            }

            return hashValue;
        }
    };
};
