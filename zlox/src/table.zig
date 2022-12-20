const std = @import("std");
const debug = @import("debug.zig");
const Obj = @import("object.zig").Obj;
const GC = @import("memory.zig").GC;
const Value = @import("value.zig").Value;
const Allocator = std.mem.Allocator;

const Entry = struct {
    key: ?*Obj.String,
    value: Value,
};

pub const Table = struct {
    const MAX_LOAD = 0.75;

    allocator: Allocator,
    entries: []Entry, // The capacity of our table is simply the length of entries.
    count: usize,

    pub fn create(allocator: Allocator) Table {
        return Table{
            .allocator = allocator,
            .entries = &[_]Entry{},
            .count = 0,
        };
    }

    pub fn destroy(self: *Table) void {
        self.allocator.free(self.entries);
    }

    fn increaseCapacity(self: *Table) !void {
        const capacity = self.entries.len;
        const newCapacity = if (capacity < 8) 8 else capacity * 2;
        const entries = try self.allocator.alloc(Entry, newCapacity);

        for (entries) |*entry| {
            entry.key = null;
            entry.value = Value.nil();
        }

        self.count = 0;
        for (self.entries) |entry| {
            // Only want to copy over the entry if the key is not null.
            if (entry.key) |key| {
                const dest = findEntry(entries, key);
                dest.key = entry.key;
                dest.value = entry.value;

                self.count += 1;
            }
        }

        // Free the old entries and set the reference to the new array.
        self.allocator.free(self.entries);
        self.entries = entries;
    }

    fn findEntry(entries: []Entry, key: *Obj.String) *Entry {
        var index = key.hash % entries.len;
        var tombstone: ?*Entry = null;

        while (true) {
            const entry = &entries[index];

            if (entry.key) |entryKey| {
                if (entryKey == key) return entry;
            } else {
                if (entry.value == .Nil) {
                    // Entry is empty.
                    return tombstone orelse entry;
                } else {
                    // Found a tombstone.
                    if (tombstone == null) tombstone = entry;
                }
            }

            index = (index + 1) % entries.len;
        }
    }

    pub fn findString(self: *Table, chars: []const u8, hash: usize) ?*Obj.String {
        if (self.count == 0) return null;

        var index = hash % self.entries.len;
        while (true) {
            const entry = self.entries[index];
            if (entry.key) |key| {
                if (key.hash == hash and std.mem.eql(u8, key.chars, chars)) {
                    return key;
                }
            } else {
                // Stop if we find an empty and non-tombstone entry.
                if (entry.value == .Nil) return null;
            }

            index = (index + 1) % self.entries.len;
        }
    }

    pub fn get(self: *Table, key: *Obj.String) ?*Value {
        if (self.count == 0) return null;

        const entry = findEntry(self.entries, key);
        if (entry.key == null) return null;

        return &entry.value;
    }

    pub fn set(self: *Table, key: *Obj.String, value: Value) !bool {
        // Enforce a max load of 75%. Below is done to avoid type conversion.
        if (4 * (self.count + 1) > 3 * self.entries.len) {
            try self.increaseCapacity();
        }

        const entry = findEntry(self.entries, key);
        const isNewKey = entry.key == null;
        if (isNewKey and entry.value == .Nil) self.count += 1;

        entry.key = key;
        entry.value = value;

        return isNewKey;
    }

    pub fn delete(self: *Table, key: *Obj.String) bool {
        if (self.count == 0) return false;

        const entry = findEntry(self.entries, key);
        if (entry.key == null) return false;

        // Place a tombstone in the entry to not impact any subsequent probing.
        entry.key = null;
        entry.value = Value.fromBool(true);

        return true;
    }

    pub fn addAll(from: *Table, to: *Table) !void {
        for (from.entries) |entry| {
            if (entry.key) |key| {
                _ = try to.set(key, entry.value);
            }
        }
    }

    pub fn print(self: *Table) void {
        debug.print("== Printing table entries ==\n", .{});
        for (self.entries) |entry| {
            if (entry.key) |key| {
                debug.print("{s} -> {s}\n", .{ key.chars, entry.value });
            }
        }
        debug.print("============================\n", .{});
    }
};
