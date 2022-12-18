// Taken from another implementation of Lox in Zig
// https://github.com/jwmerrill/zig-lox/blob/master/src/stack.zig
const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn FixedCapacityStack(comptime T: type) type {
    return struct {
        allocator: Allocator,
        buffer: []T,
        items: []T,

        const Self = @This();

        pub fn create(allocator: Allocator, comptime capacity: usize) !Self {
            var buffer = try allocator.alloc(T, capacity);

            return Self{
                .allocator = allocator,
                .buffer = buffer,
                .items = buffer[0..0],
            };
        }

        pub fn destroy(self: *Self) void {
            self.allocator.free(self.buffer);
        }

        pub fn push(self: *Self, item: T) void {
            std.debug.assert(self.items.len < self.buffer.len);

            self.items = self.buffer[0 .. self.items.len + 1];
            self.items[self.items.len - 1] = item;
        }

        pub fn pop(self: *Self) T {
            std.debug.assert(self.items.len != 0);

            const value = self.items[self.items.len - 1];
            self.items = self.buffer[0 .. self.items.len - 1];

            return value;
        }

        pub fn resize(self: *Self, newSize: usize) void {
            std.debug.assert(self.buffer.len >= newSize);

            self.items = self.buffer[0..newSize];
        }
    };
}
