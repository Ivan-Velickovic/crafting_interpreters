const std = @import("std");
const debug_options = @import("debug_options");
const compiler = @import("compiler.zig");
const debug = @import("debug.zig");
const main = @import("main.zig");
const GC = @import("memory.zig").GC;
const Compiler = @import("compiler.zig").Compiler;
const Table = @import("table.zig").Table;
const Value = @import("value.zig").Value;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Obj = @import("object.zig").Obj;
const FixedCapacityStack = @import("stack.zig").FixedCapacityStack;
const Allocator = std.mem.Allocator;

const BoundMethod = Obj.BoundMethod;
const Class = Obj.Class;
const Closure = Obj.Closure;
const Function = Obj.Function;
const Instance = Obj.Instance;
const Native = Obj.Native;
const String = Obj.String;
const Upvalue = Obj.Upvalue;

const stdout = main.stdout;
const stderr = main.stderr;

pub const InterpretError = error{
    CompileError,
    RuntimeError,
};

pub const CallFrame = struct {
    closure: *Obj.Closure,
    ip: usize,
    /// Index into the VM stack where values for this CallFrame start.
    start: usize,
};

pub const VM = struct {
    const FRAMES_MAX = 64;
    const STACK_MAX = FRAMES_MAX * (Compiler.MAX_LOCALS + 1);

    allocator: Allocator,
    frames: [FRAMES_MAX]CallFrame,
    frame_count: u7,
    stack: FixedCapacityStack(Value),
    globals: Table,
    strings: Table,
    init_string: ?*String,
    open_upvalues: ?*Upvalue,
    bytes_allocated: usize,
    next_gc: usize,
    objects: ?*Obj,
    gray_stack: std.ArrayList(*Obj),

    pub fn create() !VM {
        var vm = VM{
            .allocator = undefined,
            .frames = undefined,
            .frame_count = 0,
            .stack = undefined,
            .globals = undefined,
            .strings = undefined,
            .init_string = null,
            .open_upvalues = null,
            .bytes_allocated = 0,
            .next_gc = 1024 * 1024,
            .objects = null,
            .gray_stack = undefined,
        };

        return vm;
    }

    pub fn init(self: *VM, gc: *GC) !void {
        const gc_allocator = gc.allocator();
        self.allocator = gc_allocator;
        self.stack = try FixedCapacityStack(Value).create(gc_allocator, STACK_MAX);
        self.globals = Table.create(gc_allocator);
        self.strings = Table.create(gc_allocator);
        // Note that to keep track of the gray objects, we do not allocate from the GC,
        // instead we surpass it by allocating straight from the internal allocator. This
        // is so that the memory for the gray stack is not managed by the GC to prevent
        // growing the gray stack causing recursive invocations of the GC if we were in
        // the middle of a garbage collection.
        self.gray_stack = std.ArrayList(*Obj).init(gc.internalAllocator);

        self.init_string = try String.copy(self, "init");

        try self.defineNative("clock", clockNative);
    }

    pub fn destroy(self: *VM) void {
        self.stack.destroy();
        self.globals.destroy();
        self.strings.destroy();
        self.init_string = null;
        self.freeObjects();
        self.gray_stack.deinit();
    }

    fn freeObjects(self: *VM) void {
        var object = self.objects;
        while (object) |obj| {
            const next = obj.next;
            obj.destroy(self);
            object = next;
        }
    }

    pub fn interpret(self: *VM, source: []const u8) !void {
        const function = try compiler.compile(source, self);

        self.stack.push(Value.fromObj(&function.obj));
        const closure = try Obj.Closure.create(self, function);
        // Pop the function that compile returns and push the closure that wraps it.
        _ = self.stack.pop();
        self.stack.push(Value.fromObj(&closure.obj));
        try self.call(closure, 0);

        try self.run();
    }

    fn runtimeError(self: *VM, comptime fmt: []const u8, args: anytype) !void {
        try stderr.print(fmt ++ "\n", args);

        var i: usize = 0;
        while (i < self.frame_count) : (i += 1) {
            const frame = &self.frames[self.frame_count - 1 - i];

            // frame.ip - 1 because we want to refer to the previous instruction as
            // that is where the error occurred.
            const line = frame.closure.function.chunk.lines.items[frame.ip - 1];
            try stderr.print("[line {}] in ", .{line});

            if (frame.closure.function.name) |name| {
                try stderr.print("{s}()\n", .{name.chars});
            } else {
                try stderr.print("script\n", .{});
            }
        }

        self.resetStack();
    }

    fn defineNative(self: *VM, name: []const u8, function: Native.Fn) !void {
        const name_obj = &(try String.copy(self, name)).obj;
        self.stack.push(Value.fromObj(name_obj));
        const function_obj = &(try Native.create(self, function)).obj;
        self.stack.push(Value.fromObj(function_obj));

        _ = try self.globals.set(self.stack.items[0].Obj.asType(String), self.stack.items[1]);
        _ = self.stack.pop();
        _ = self.stack.pop();
    }

    fn resetStack(self: *VM) void {
        self.stack.resize(0);
        self.frame_count = 0;
        self.open_upvalues = null;
    }

    fn peek(self: *VM, distance: usize) Value {
        return self.stack.items[self.stack.items.len - distance - 1];
    }

    fn call(self: *VM, closure: *Closure, arg_count: u8) !void {
        if (arg_count != closure.function.arity) {
            try self.runtimeError("Expected {d} arguments but got {d}.", .{ closure.function.arity, arg_count });
            return InterpretError.RuntimeError;
        }

        if (self.frame_count == FRAMES_MAX) {
            try self.runtimeError("Stack overflow.", .{});
            return InterpretError.RuntimeError;
        }

        self.frames[self.frame_count] = CallFrame{
            .closure = closure,
            .ip = 0,
            // We want the slots to start from the beggining of the bytecode
            // for a function call, so we start one before its arguments since
            // that also includes the name of the function.
            .start = self.stack.items.len - arg_count - 1,
        };
        self.frame_count += 1;
    }

    fn callValue(self: *VM, callee: Value, arg_count: u8) !void {
        if (callee == .Obj) {
            switch (callee.Obj.obj_type) {
                .BoundMethod => {
                    const bound_method = callee.Obj.asType(BoundMethod);
                    self.stack.items[self.stack.items.len - arg_count - 1] = bound_method.receiver;
                    return try self.call(bound_method.method, arg_count);
                },
                .Class => {
                    const class = callee.Obj.asType(Class);
                    const instance = try Obj.Instance.create(self, class);
                    self.stack.items[self.stack.items.len - arg_count - 1] = Value.fromObj(&instance.obj);
                    if (class.methods.get(self.init_string.?)) |initialiser| {
                        return try self.call(initialiser.Obj.asType(Closure), arg_count);
                    } else if (arg_count != 0) {
                        // If we do not find an initialiser for the class and
                        // we do not expect to get a class being called with
                        // arguments.
                        try self.runtimeError("Expected 0 arguments but got {}.", .{ arg_count });
                        return InterpretError.RuntimeError;
                    }
                    return;
                },
                .Closure => return try self.call(callee.Obj.asType(Closure), arg_count),
                .Native => {
                    const native = callee.Obj.asType(Native).function;
                    const result = native(arg_count, self.stack.items[self.stack.items.len - arg_count..]);

                    self.stack.resize(self.stack.items.len - arg_count + 1);
                    self.stack.push(result);

                    return;
                },
                else => {},
            }
        }

        try self.runtimeError("Can only call functions and classes.", .{});

        return InterpretError.RuntimeError;
    }

    fn invokeFromClass(self: *VM, class: *Class, method_name: *String, arg_count: u8) !void {
        if (class.methods.get(method_name)) |method| {
            try self.call(method.Obj.asType(Closure), arg_count);
        } else {
            try self.runtimeError("Undefined property '{s}'.", .{ method_name.chars });
            return InterpretError.RuntimeError;
        }
    }

    fn invoke(self: *VM, method_name: *String, arg_count: u8) !void {
        const receiver = self.peek(arg_count);
        if (!Obj.isType(receiver, .Instance)) {
            try self.runtimeError("Only instances have methods.", .{});
            return InterpretError.RuntimeError;
        }

        const instance = receiver.Obj.asType(Instance);

        // The method_name could actually be a field and not a method, so first
        // we need to check whether the field exists and if so, check if it is
        // the kind value we can call.
        if (instance.fields.get(method_name)) |value| {
            self.stack.items[self.stack.items.len - arg_count - 1] = value.*;
            try self.callValue(value.*, arg_count);
        } else {
            try self.invokeFromClass(instance.class, method_name, arg_count);
        }
    }

    fn bindMethod(self: *VM, class: *Class, name: *String) !bool {
        if (class.methods.get(name)) |method| {
            // We grab the receiver of the bound method (the instance), which is
            // implemented as a closure, from the top of the stack. We then pop off
            // the instance and then replace the top of the stack with the bound
            // method we just created.
            const bound_method = try Obj.BoundMethod.create(self, self.peek(0), method.Obj.asType(Closure));
            _ = self.stack.pop();
            self.stack.push(Value.fromObj(&bound_method.obj));
            return true;
        } else {
            try self.runtimeError("Undefined property '{s}'.", .{ name.chars });
            return false;
        }
    }

    fn captureUpvalue(self: *VM, local: *Value) !*Upvalue {
        var prev_upvalue: ?*Upvalue = null;
        var curr_upvalue = self.open_upvalues;
        while (curr_upvalue) |upvalue| {
            // Since open upvalues are ordered, we iterate past upvalues, starting from the
            // top of the stack, that point to Values above the one we are looking for.
            if (@ptrToInt(upvalue.location) <= @ptrToInt(local)) break;

            prev_upvalue = upvalue;
            curr_upvalue = upvalue.next;
        }

        if (curr_upvalue != null and curr_upvalue.?.location == local) return curr_upvalue.?;

        var upvalue = try Upvalue.create(self, local);
        // If we got to here, it means that local is further up the stack than where upvalue
        // points to, so we want to insert the newly created upvalue before curr_upvalue.
        upvalue.next = curr_upvalue;

        if (prev_upvalue) |prev| {
            prev.next = upvalue;
        } else {
            self.open_upvalues = upvalue;
        }

        return upvalue;
    }

    fn closeUpvalues(self: *VM, last: *Value) void {
        while (self.open_upvalues) |upvalue| {
            // We want to close every open upvalue that is above the given slot in the stack.
            if (@ptrToInt(upvalue.location) < @ptrToInt(last)) break;
            // We move the Value at location from the stack to the heap allocated Upvalue,
            // we then change location to point to this "closed" value so that references
            // to the location of an upvalue are still correct.
            upvalue.closed = upvalue.location.*;
            upvalue.location = &upvalue.closed;
            self.open_upvalues = upvalue.next;
        }
    }

    fn defineMethod(self: *VM, name: *String) !void {
        // The method closure is on top of the stack currently. Below the
        // closure will be the class it will be bound to. We take those two
        // stack slots and store the closure in the class's method table.
        // Finally, we pop the clousre since we no longer need it.
        const method = self.peek(0);
        const class = self.peek(1).Obj.asType(Class);
        _ = try class.methods.set(name, method);
        _ = self.stack.pop();
    }

    fn readString(frame: *CallFrame) *String {
        return readConstant(frame).Obj.asType(String);
    }

    fn readConstant(frame: *CallFrame) Value {
        return frame.closure.function.chunk.constants.items[readByte(frame)];
    }

    fn readByte(frame: *CallFrame) u8 {
        defer frame.ip += 1;

        return frame.closure.function.chunk.code.items[frame.ip];
    }

    fn readShort(frame: *CallFrame) u16 {
        frame.ip += 2;
        const hi = @as(u16, frame.closure.function.chunk.code.items[frame.ip - 2]);
        const lo = frame.closure.function.chunk.code.items[frame.ip - 1];

        return (hi << 8) | lo;
    }

    fn binaryBooleanOp(self: *VM, op: OpCode) !void {
        const rhs = self.stack.pop();
        const lhs = self.stack.pop();

        if (lhs != .Number or rhs != .Number) {
            try self.runtimeError("Operands must be numbers.", .{});
            return InterpretError.RuntimeError;
        }

        const result = switch (op) {
            .Greater => lhs.Number > rhs.Number,
            .Less => lhs.Number < rhs.Number,
            else => unreachable
        };

        self.stack.push(Value.fromBool(result));
    }

    fn binaryNumericOp(self: *VM, op: OpCode) !void {
        const rhs = self.stack.pop();
        const lhs = self.stack.pop();

        if (lhs != .Number or rhs != .Number) {
            try self.runtimeError("Operands must be numbers.", .{});
            return InterpretError.RuntimeError;
        }

        const result = switch (op) {
            .Subtract => lhs.Number - rhs.Number,
            .Multiply => lhs.Number * rhs.Number,
            .Divide => lhs.Number / rhs.Number,
            else => unreachable
        };

        self.stack.push(Value.fromNumber(result));
    }

    // TODO: push back onto the stack or just do the peek/pop in here?
    fn concatenate(self: *VM, a: *String, b: *String) !void {
        self.stack.push(Value.fromObj(&a.obj));
        self.stack.push(Value.fromObj(&b.obj));

        const slices = [_][]const u8 {a.chars, b.chars};
        const concatenated_chars = try std.mem.concat(self.allocator, u8, &slices);

        _ = self.stack.pop();
        _ = self.stack.pop();

        const result = try String.take(self, concatenated_chars);
        self.stack.push(Value.fromObj(&result.obj));
    }

    pub fn run(self: *VM) !void {
        var frame = &self.frames[self.frame_count - 1];

        while (true) {
            if (debug_options.trace_execution) {
                try stdout.print("          ", .{});
                for (self.stack.items) |value| {
                    try stdout.print("[ {} ]", .{value});
                }

                try stdout.print("\n", .{});
                _ = try debug.disassembleInstruction(&frame.closure.function.chunk, frame.ip);
            }

            const opCode = @intToEnum(OpCode, readByte(frame));
            switch (opCode) {
                .Constant => self.stack.push(readConstant(frame)),
                .Nil => self.stack.push(Value.nil()),
                .True => self.stack.push(Value.fromBool(true)),
                .False => self.stack.push(Value.fromBool(false)),
                .Pop => _ = self.stack.pop(),
                .GetLocal => {
                    const slot = readByte(frame);
                    self.stack.push(self.stack.items[frame.start + slot]);
                },
                .SetLocal => {
                    const slot = readByte(frame);
                    self.stack.items[frame.start + slot] = self.peek(0);
                },
                .GetGlobal => {
                    const name = readString(frame);
                    if (self.globals.get(name)) |value| {
                        self.stack.push(value.*);
                    } else {
                        try self.runtimeError("Undefined variable '{s}'.", .{ name.chars });
                        return InterpretError.RuntimeError;
                    }
                },
                .DefineGlobal => {
                    const name = readString(frame);
                    _ = try self.globals.set(name, self.peek(0));
                    _ = self.stack.pop();
                },
                .SetGlobal => {
                    const name = readString(frame);
                    if (try self.globals.set(name, self.peek(0))) {
                        _ = self.globals.delete(name);
                        try self.runtimeError("Undefined variable '{s}'.", .{ name.chars });
                        return InterpretError.RuntimeError;
                    }
                },
                .GetUpvalue => {
                    const slot = readByte(frame);
                    self.stack.push(frame.closure.upvalues.items[slot].location.*);
                },
                .SetUpvalue => {
                    const slot = readByte(frame);
                    frame.closure.upvalues.items[slot].location.* = self.peek(0);
                },
                .GetProperty => {
                    if (!Obj.isType(self.peek(0), .Instance)) {
                        try self.runtimeError("Only instances have properties.", .{});
                        return InterpretError.RuntimeError;
                    }

                    const instance = self.peek(0).Obj.asType(Instance);
                    const field_name = readString(frame);

                    if (instance.fields.get(field_name)) |value| {
                        _ = self.stack.pop(); // Pop the instance.
                        self.stack.push(value.*);
                    } else {
                        if (!try self.bindMethod(instance.class, field_name)) {
                            return InterpretError.RuntimeError;
                        }
                    }
                },
                .SetProperty => {
                    if (!Obj.isType(self.peek(1), .Instance)) {
                        try self.runtimeError("Only instances have fields.", .{});
                        return InterpretError.RuntimeError;
                    }

                    // Here the current state of the stack has the class
                    // instance below the value to be set to the field. Since
                    // setting a field itself is an expression, we want to pop
                    // off the instance (by popping the field value first) and
                    // then push the field value back on to the stack.
                    // An example below:
                    //
                    // class Bread {}
                    // var bread = Bread();
                    // print bread.spread = "marmite";
                    //
                    // In addition to setting the field, the last line will
                    // print "marmite";
                    const instance = self.peek(1).Obj.asType(Instance);
                    const field_name = readString(frame);
                    _ = try instance.fields.set(field_name, self.peek(0));
                    const field_value = self.stack.pop();
                    _ = self.stack.pop();
                    self.stack.push(field_value);
                },
                .Equal => {
                    const rhs = self.stack.pop();
                    const lhs = self.stack.pop();
                    self.stack.push(Value.fromBool(Value.isEqual(lhs, rhs)));
                },
                .Greater, .Less => try self.binaryBooleanOp(opCode),
                .Subtract, .Multiply, .Divide => try self.binaryNumericOp(opCode),
                .Add => {
                    const rhs = self.stack.pop();
                    const lhs = self.stack.pop();

                    if (Obj.isType(lhs, .String) and Obj.isType(rhs, .String)) {
                        try self.concatenate(lhs.Obj.asType(String), rhs.Obj.asType(String));
                    } else if (lhs == .Number and rhs == .Number) {
                        self.stack.push(Value.fromNumber(lhs.Number + rhs.Number));
                    } else {
                        try self.runtimeError("Operands must be two numbers or two strings.", .{});
                        return InterpretError.RuntimeError;
                    }
                },
                .Not => {
                    const value = self.stack.pop();
                    self.stack.push(Value.fromBool(value.isFalsey()));
                },
                .Negate => {
                    const value = self.stack.pop();
                    if (value != .Number) {
                        try self.runtimeError("Operand must be a number.", .{});
                        return InterpretError.RuntimeError;
                    }

                    self.stack.push(Value.fromNumber(-value.Number));
                },
                .Print => try stdout.print("{}\n", .{self.stack.pop()}),
                .Jump => {
                    const offset = readShort(frame);
                    frame.ip += offset;
                },
                .JumpIfFalse => {
                    const offset = readShort(frame);
                    if (self.peek(0).isFalsey()) frame.ip += offset;
                },
                .Loop => {
                    const offset = readShort(frame);
                    frame.ip -= offset;
                },
                .Call => {
                    const arg_count = readByte(frame);
                    try self.callValue(self.peek(arg_count), arg_count);
                    // Since calling a function will create a new CallFrame on the frames stack,
                    // so we update the frame that is being interpreted.
                    frame = &self.frames[self.frame_count - 1];
                },
                .Invoke => {
                    const method = readString(frame);
                    const arg_count = readByte(frame);
                    try self.invoke(method, arg_count);

                    frame = &self.frames[self.frame_count - 1];
                },
                .Closure => {
                    const function = readConstant(frame).Obj.asType(Function);
                    const closure = try Closure.create(self, function);
                    self.stack.push(Value.fromObj(&closure.obj));

                    var i: usize = 0;
                    while (i < function.upvalueCount) : (i += 1) {
                        const isLocal = readByte(frame) == 1;
                        const index = readByte(frame);
                        if (isLocal) {
                            const upvalue = try self.captureUpvalue(&self.stack.items[frame.start + index]);
                            try closure.upvalues.append(upvalue);
                        } else {
                            try closure.upvalues.append(frame.closure.upvalues.items[index]);
                        }
                    }
                },
                .CloseUpvalue => {
                    self.closeUpvalues(&self.stack.items[self.stack.items.len - 1]);
                    _ = self.stack.pop();
                },
                .Return => {
                    const result = self.stack.pop();
                    self.closeUpvalues(&self.stack.items[frame.start]);
                    self.frame_count -= 1;
                    if (self.frame_count == 0) {
                        // pop the top-level "main" function if this was the last CallFrame.
                        _ = self.stack.pop();
                        return;
                    }

                    // Since we want to get rid of everything to do with this CallFrame that
                    // is being discarded, we resize the stack such that it now ends at the
                    // element before this CallFrame started.
                    self.stack.resize(frame.start);

                    self.stack.push(result);
                    frame = &self.frames[self.frame_count - 1];
                },
                .Class => {
                    const class = try Class.create(self, readString(frame));
                    self.stack.push(Value.fromObj(&class.obj));
                },
                .Method => {
                    try self.defineMethod(readString(frame));
                },
            }
        }
    }
};

// Built-in functions for Lox programs.

// `clock()` - Timestamp in seconds, relative to UTC 1970-01-01 (as specified in Zig's stdlib).
fn clockNative(_: u8, _: []Value) Value {
    return Value.fromNumber(@intToFloat(f64, std.time.timestamp()));
}
