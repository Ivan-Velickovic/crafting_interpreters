const std = @import("std");
const zlox_options = @import("zlox_options");
const Obj = @import("object.zig").Obj;
const BoundMethod = Obj.BoundMethod;
const Class = Obj.Class;
const Closure = Obj.Closure;
const Function = Obj.Function;
const String = Obj.String;
const Instance = Obj.Instance;

pub const Value = if (zlox_options.nan_boxing) ValueNanBoxed else ValueUnion;

const ValueType = enum {
    Bool,
    Nil,
    Number,
    Obj,
};

const ValueUnion = union(ValueType) {
    Bool: bool,
    Nil: void,
    Number: f64,
    Obj: *Obj,

    pub fn isNil(self: ValueUnion) bool {
        return self == .Nil;
    }

    pub fn isBool(self: ValueUnion) bool {
        return self == .Bool;
    }

    pub fn isNumber(self: ValueUnion) bool {
        return self == .Number;
    }

    pub fn isObj(self: ValueUnion) bool {
        return self == .Obj;
    }

    pub fn isFalsey(self: ValueUnion) bool {
        return self.isNil() or (self.isBool() and !self.asBool());
    }

    pub fn asBool(self: ValueUnion) bool {
        return self.Bool;
    }

    pub fn asNumber(self: ValueUnion) f64 {
        return self.Number;
    }

    pub fn asObj(self: ValueUnion) *Obj {
        return self.Obj;
    }

    pub fn fromBool(b: bool) ValueUnion {
        return ValueUnion{ .Bool = b };
    }

    pub fn fromObj(obj: *Obj) ValueUnion {
        return ValueUnion{ .Obj = obj };
    }

    pub fn fromNumber(number: f64) ValueUnion {
        return ValueUnion{ .Number = number };
    }

    pub fn nil() ValueUnion {
        return .Nil;
    }

    pub fn isEqual(a: ValueUnion, b: ValueUnion) bool {
        if (@enumToInt(a) != @enumToInt(b)) return false;

        return switch (a) {
            .Bool => a.Bool == b.Bool,
            .Nil => true,
            .Number => a.Number == b.Number,
            .Obj => a.Obj == b.Obj,
        };
    }

    pub fn format(
        self: ValueUnion,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype
    ) !void {
        switch (self) {
            .Bool => try writer.print("{}", .{ self.Bool }),
            .Nil => try writer.print("nil", .{}),
            .Number => try writer.print("{d}", .{ self.Number }),
            .Obj => {
                const obj = self.Obj;
                switch (obj.obj_type) {
                    .BoundMethod => {
                        const name = if (obj.asType(BoundMethod).method.function.name) |name| name.chars else "<script>";
                        try writer.print("<fn {s}>", .{ name });
                    },
                    // TODO: we want a bound method to be formatted the same way a Function is, but I can't figure out how to actually call this function recurisively.
                    // Seems like doing try writer.print on the function doesn't work, neither deos below, in fact it seems to make teh compiler go in an infinite loop.
                    // .BoundMethod => try format(obj.asType(BoundMethod).method.function, .{}, .{}, writer),
                    .Class => try writer.print("{s}", .{ obj.asType(Class).name.chars }),
                    .String => try writer.print("{s}", .{ obj.asType(String).chars }),
                    .Function => {
                        const name = if (obj.asType(Function).name) |name| name.chars else "<script>";
                        try writer.print("<fn {s}>", .{ name });
                    },
                    .Instance => try writer.print("{s} instance", .{ obj.asType(Instance).class.name.chars }),
                    .Native => try writer.print("<native fn>", .{}),
                    .Closure => {
                        const name = if (obj.asType(Closure).function.name) |name| name.chars else "<script>";
                        try writer.print("<fn {s}>", .{ name });
                    },
                    .Upvalue => try writer.print("upvalue", .{}),
                }
            }
        }
    }
};

// TODO: do we need packed here?
const ValueNanBoxed = packed struct {
    // TODO: explain these
    const NAN_BITS: u64 = 0b0111111111111100000000000000000000000000000000000000000000000000;
    const SIGN_BIT: u64 = 0b1000000000000000000000000000000000000000000000000000000000000000;
    const TAG_NIL: u64 = 0b01;
    const TAG_FALSE: u64 = 0b10;
    const TAG_TRUE: u64 = 0b11;

    const NIL_BITS: u64 = NAN_BITS | TAG_NIL;
    const FALSE_BITS: u64 = NAN_BITS | TAG_FALSE;
    const TRUE_BITS: u64 = NAN_BITS | TAG_TRUE;
    const OBJ_BITS: u64 = SIGN_BIT | NAN_BITS;

    bits: u64,

    pub fn isNil(self: ValueNanBoxed) bool {
        return self.bits == NIL_BITS;
    }

    pub fn isBool(self: ValueNanBoxed) bool {
        return self.bits | 1 == TRUE_BITS;
    }

    pub fn isNumber(self: ValueNanBoxed) bool {
        return self.bits & NAN_BITS != NAN_BITS;
    }

    pub fn isObj(self: ValueNanBoxed) bool {
        return self.bits & OBJ_BITS == OBJ_BITS;
    }

    pub fn isFalsey(self: ValueNanBoxed) bool {
        return self.isNil() or (self.isBool() and !self.asBool());
    }

    pub fn fromBool(b: bool) ValueNanBoxed {
        return if (b) ValueNanBoxed{ .bits = TRUE_BITS } else ValueNanBoxed{ .bits = FALSE_BITS };
    }

    pub fn fromObj(obj: *Obj) ValueNanBoxed {
        // Check that only the bottom 48-bits are used by the pointer so that we
        // retain the correct address once it is put into a NaN-boxed value.
        std.debug.assert(OBJ_BITS & @ptrToInt(obj) == 0);
        return ValueNanBoxed{ .bits = @ptrToInt(obj) | OBJ_BITS };
    }

    pub fn fromNumber(number: f64) ValueNanBoxed {
        return @bitCast(ValueNanBoxed, number);
    }

    pub fn asNumber(self: ValueNanBoxed) f64 {
        return @bitCast(f64, self.bits);
    }

    pub fn asBool(self: ValueNanBoxed) bool {
        return self.bits == TRUE_BITS;
    }

    pub fn asObj(self: ValueNanBoxed) *Obj {
        return @intToPtr(*Obj, self.bits & ~OBJ_BITS);
    }

    pub fn nil() ValueNanBoxed {
        return ValueNanBoxed{ .bits = NIL_BITS };
    }

    pub fn isEqual(a: ValueNanBoxed, b: ValueNanBoxed) bool {
        // TODO: explain this
        if (a.isNumber() and b.isNumber()) {
            return a.asNumber() == b.asNumber();
        }

        return a.bits == b.bits;
    }

    pub fn format(
        self: ValueNanBoxed,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype
    ) !void {
        if (self.isBool()) {
            try writer.print("{}", .{ self.asBool() });
        } else if (self.isNil()) {
            try writer.print("nil", .{});
        } else if (self.isNumber()) {
            try writer.print("{d}", .{ self.asNumber() });
        } else if (self.isObj()) {
            try printObject(self, writer);
        } else {
            unreachable;
        }
    }
};

fn printObject(
    self: ValueNanBoxed,
    writer: anytype
) !void {
    const obj = self.asObj();
    switch (obj.obj_type) {
        .BoundMethod => {
            const name = if (obj.asType(BoundMethod).method.function.name) |name| name.chars else "<script>";
            try writer.print("<fn {s}>", .{ name });
        },
        // TODO: we want a bound method to be formatted the same way a Function is, but I can't figure out how to actually call this function recurisively.
        // Seems like doing try writer.print on the function doesn't work, neither deos below, in fact it seems to make teh compiler go in an infinite loop.
        // .BoundMethod => try format(obj.asType(BoundMethod).method.function, .{}, .{}, writer),
        .Class => try writer.print("{s}", .{ obj.asType(Class).name.chars }),
        .String => try writer.print("{s}", .{ obj.asType(String).chars }),
        .Function => {
            const name = if (obj.asType(Function).name) |name| name.chars else "<script>";
            try writer.print("<fn {s}>", .{ name });
        },
        .Instance => try writer.print("{s} instance", .{ obj.asType(Instance).class.name.chars }),
        .Native => try writer.print("<native fn>", .{}),
        .Closure => {
            const name = if (obj.asType(Closure).function.name) |name| name.chars else "<script>";
            try writer.print("<fn {s}>", .{ name });
        },
        .Upvalue => try writer.print("upvalue", .{}),
    }
}
