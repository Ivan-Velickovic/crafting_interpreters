const std = @import("std");
const Obj = @import("object.zig").Obj;

const ValueType = enum {
    Bool,
    Nil,
    Number,
    Obj,
};

pub const Value = union(ValueType) {
    Bool: bool,
    Nil: void,
    Number: f64,
    Obj: *Obj,

    pub fn isFalsey(self: Value) bool {
        return self == .Nil or (self == .Bool and !self.Bool);
    }

    pub fn fromBool(b: bool) Value {
        return Value{ .Bool = b };
    }

    pub fn fromObj(obj: *Obj) Value {
        return Value{ .Obj = obj };
    }

    pub fn fromNumber(number: f64) Value {
        return Value{ .Number = number };
    }

    pub fn nil() Value {
        return .Nil;
    }

    pub fn isEqual(a: Value, b: Value) bool {
        if (@enumToInt(a) != @enumToInt(b)) return false;

        return switch (a) {
            .Bool => a.Bool == b.Bool,
            .Nil => true,
            .Number => a.Number == b.Number,
            .Obj => a.Obj == b.Obj,
        };
    }

    pub fn format(
        self: Value,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) !void {
        switch (self) {
            .Bool => try writer.print("{}", .{self.Bool}),
            .Nil => try writer.print("nil", .{}),
            .Number => try writer.print("{d}", .{self.Number}),
            .Obj => {
                switch (self.Obj.objType) {
                    .String => try writer.print("{s}", .{self.Obj.asType(Obj.String).chars}),
                }
            }
        }
    }
};
