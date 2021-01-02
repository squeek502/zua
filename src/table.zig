const std = @import("std");
const object = @import("object.zig");
const Value = object.Value;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

// Notes:
//
// Lua tables are a combination of an array and a hash table
// where contiguous integer keys from 1..n are stored in an array
// while any other keys are put into the hash table
//
// Lua's table implementation uses a lua_State but as far as
// I can tell it is only ever used to allocate memory, so
// we can omit that dependency here and just pass an allocator
// around instead
//
// TODO: Some functions (like luaH_next) push things onto the Lua stack.
//       This could potentially be dealt with through an intermediate function
//       to keep Table ignorant of the stack/lua_State.
//
// TODO: There seem to be quite a few quirks specific to the particular implementation
//       of the hybrid array/hashmap data structure that Lua uses for its tables.
//       Using Zig's std lib might prove to be too different, and a port of the
//       Lua implementation might end up being necessary to keep compatibility.

pub const Table = struct {
    array: ArrayPortion,
    map: MapPortion.HashMap,
    allocator: *Allocator,

    const ArrayPortion = std.ArrayList(Value);
    const MapPortion = struct {
        pub const HashMap: type = std.ArrayHashMap(Value, Value, keyHash, keyEql, false);

        // TODO: This is woefully underimplemented
        fn keyHash(key: Value) u32 {
            switch (key) {
                .boolean => |val| {
                    const autoHashFn = std.array_hash_map.getAutoHashFn(@TypeOf(val));
                    return autoHashFn(val);
                },
                .number => |val| {
                    const floatBits = @typeInfo(@TypeOf(val)).Float.bits;
                    const hashType = std.meta.Int(.unsigned, floatBits);
                    const autoHashFn = std.array_hash_map.getAutoHashFn(hashType);
                    return autoHashFn(@bitCast(hashType, val));
                },
                .string, .table, .function, .userdata, .thread => |val| {
                    // TODO
                    //const ptrHashFn = std.hash_map.getHashPtrAddrFn(*object.GCObject);
                    //return ptrHashFn(val);
                    return 0;
                },
                .light_userdata => |val| {
                    const ptrHashFn = std.array_hash_map.getHashPtrAddrFn(@TypeOf(val));
                    return ptrHashFn(val);
                },
                .nil => {
                    // TODO: nil key will lead to a 'table index is nil' error
                    // so might be able to short-circuit here instead
                    return 0;
                },
                .none => unreachable,
            }
        }

        fn keyEql(a: Value, b: Value) bool {
            if (a.getType() != b.getType()) {
                return false;
            }

            // TODO: type-specific eql
            const autoEqlFn = std.array_hash_map.getAutoEqlFn(Value);
            return autoEqlFn(a, b);
        }
    };

    pub fn init(allocator: *Allocator) Table {
        // no initial capacity removes the chance of failure
        return initCapacity(allocator, 0, 0) catch unreachable;
    }

    pub fn initCapacity(allocator: *Allocator, narray: usize, nhash: usize) !Table {
        var self = Table{
            .array = ArrayPortion.init(allocator),
            .map = MapPortion.HashMap.init(allocator),
            .allocator = allocator,
        };
        errdefer self.deinit();
        if (narray > 0) {
            // this is kinda weird, but it more closely matches how Lua expects things
            // to work
            try self.array.appendNTimes(Value.nil, narray);
        }
        if (nhash > 0) {
            try self.map.ensureCapacity(nhash);
        }
        return self;
    }

    pub fn deinit(self: *Table) void {
        self.array.deinit();
        self.map.deinit();
    }

    /// returns the index for `key' if `key' is an appropriate key to live in
    /// the array part of the table, error.InvalidType otherwise.
    pub fn arrayIndex(key: Value) !usize {
        if (key != .number) {
            return error.InvalidArrayKey;
        }
        const float_val = key.number;
        // TODO: better handling of float vals that can't be converted to usize (out of range, negative, etc)
        const int_val = @floatToInt(usize, float_val);
        // must be positive and the float and int version of the key must be the same
        if (int_val < 0 or @intToFloat(f64, int_val) != float_val) {
            return error.InvalidArrayKey;
        }
        return int_val;
    }

    pub fn toLuaIndex(index: usize) usize {
        return index + 1;
    }

    pub fn fromLuaIndex(index: usize) usize {
        assert(index > 0);
        return index - 1;
    }

    pub fn isLuaIndexInArrayBounds(self: *Table, lua_index: usize) bool {
        return lua_index > 0 and lua_index <= self.array.items.len;
    }

    pub fn arrayGet(self: *Table, lua_index: usize) ?*Value {
        if (isLuaIndexInArrayBounds(self, lua_index)) {
            return &(self.array.items[fromLuaIndex(lua_index)]);
        }
        return null;
    }

    pub fn get(self: *Table, key: Value) ?*Value {
        switch (key) {
            .nil => return null,
            .none => unreachable,
            .number => {
                // try the array portion
                if (arrayIndex(key)) |lua_i| {
                    if (self.arrayGet(lua_i)) |val| {
                        return val;
                    }
                } else |err| switch (err) {
                    error.InvalidArrayKey => {},
                }
                // fall back to the map portion
            },
            else => {},
        }
        const kv = self.map.getEntry(key);
        return if (kv != null) &(kv.?.value) else null;
    }

    // Some notes:
    // - In Lua this is luaH_set, luaH_setnum, luaH_setstr
    // - Lua basically sticks everything in the hash map part of the
    //   table and then 'rehashes' once the hash map needs to resize
    //   and only then fills up the array portion with contiguous
    //   non-nil indexes
    // - The Lua version of this function doesn't accept a value
    //   to initialize the newly added key with, it returns a pointer
    //   for the caller to initialize. This functionality is kept here
    //   (TODO: for now?)
    pub fn getOrCreate(self: *Table, key: Value) !*Value {
        // note: this will handle 'adding' things to keys with nil values
        if (self.get(key)) |val| {
            return val;
        }
        if (key == .nil) {
            return error.TableIndexIsNil;
        }
        if (key == .number and std.math.isNan(key.number)) {
            return error.TableIndexIsNan;
        }
        //if (arrayIndex(key)) |lua_i| {
        //    if (self.arrayGet(lua_i)) |val| {
        //        return val;
        //    }
        //} else |err| switch (err) {
        //    error.InvalidArrayKey => {},
        //}
        // TODO: handle growing/shrinking the array portion
        const kv = try self.map.getOrPutValue(key, Value.nil);
        return &(kv.value);
    }

    /// luaH_getn equivalent
    pub fn getn(self: *Table) usize {
        // There is a quirk in the Lua implementation of this which means that
        // the table's length is equal to the allocated size of the array if
        // the map part is empty and the last value of the array part is non-nil
        // (even if there are nil values somewhere before the last value)
        //
        // Example:
        //   tbl = {1,2,3,4,5,6}
        //   assert(#tbl == 6)
        //   tbl[3] = nil
        //   assert(#tbl == 6)
        //   tbl[6] = nil
        //   assert(#tbl == 2)
        //
        // This is a direct port of the Lua implementation to maintain
        // that quirk
        var j: usize = self.array.items.len;
        if (j > 0 and self.array.items[fromLuaIndex(j)] == .nil) {
            var i: usize = 0;
            while (j - i > 1) {
                var m: usize = (i + j) / 2;
                if (self.array.items[fromLuaIndex(m)] == .nil) {
                    j = m;
                } else {
                    i = m;
                }
            }
            return i;
        }
        // if the map portion is empty and the array portion is contiguous,
        // then just return the array portion length
        if (self.map.count() == 0) {
            return j;
        }
        // need to check the map portion for any remaining contiguous keys now
        var last_contiguous_i: usize = j;
        j += 1;
        while (self.map.get(Value{ .number = @intToFloat(f64, j) })) |value| {
            if (value == .nil) {
                break;
            }
            last_contiguous_i = j;
            j *= 2;
            // There's another quirk here where a maliciously constructed table
            // can take advantage of the binary search algorithm here to
            // erroneously inflate the calculated length of contiguous non-nil
            // indexes, but Lua will only backtrack and do a linear search if the
            // size gets out of control
            //
            // Example:
            //
            //   tbl = {1,2,nil,4,5}
            //   assert(#tbl == 5)
            //   tbl[10] = 10
            //   assert(#tbl == 10)
            //   tbl[20] = 20
            //   assert(#tbl == 20)
            //
            //   i = 40
            //   while i<2147483647 do
            //     tbl[i] = i
            //     i = i*2
            //   end
            //   assert(#tbl == 2)
            if (j > std.math.maxInt(i32)) {
                var i: usize = 1;
                linear_search: while (self.get(Value{ .number = @intToFloat(f64, i) })) |linear_val| {
                    if (linear_val.* == Value.nil) {
                        break :linear_search;
                    }
                    i += 1;
                }
                return i - 1;
            }
        }
        while (j - last_contiguous_i > 1) {
            var m: usize = (last_contiguous_i + j) / 2;
            if (self.map.get(Value{ .number = @intToFloat(f64, m) })) |value| {
                if (value != .nil) {
                    // if value is non-nil, then search values above
                    last_contiguous_i = m;
                    continue;
                }
            }
            // if value is nil, then search values below
            j = m;
        }
        return last_contiguous_i;
    }

    pub const KV = struct {
        key: Value,
        value: Value,
    };

    /// extremely loose luaH_next equivalent, minus the stack
    /// interaction
    pub fn next(self: *Table, key: Value) ?Table.KV {
        var next_i: usize = 1;
        if (arrayIndex(key)) |cur_i| {
            next_i = cur_i + 1;
        } else |err| switch (err) {
            error.InvalidArrayKey => {},
        }
        while (self.isLuaIndexInArrayBounds(next_i)) : (next_i += 1) {
            const val = self.array.items[fromLuaIndex(next_i)];
            if (val != .nil) {
                return Table.KV{
                    .key = Value{ .number = @intToFloat(f64, next_i) },
                    .value = val,
                };
            }
        }
        if (self.map.getIndex(key)) |cur_map_i| {
            next_i = cur_map_i + 1;
            const items = self.map.items();
            if (items.len > next_i) {
                const next_kv = items[next_i];
                return Table.KV{
                    .key = next_kv.key,
                    .value = next_kv.value,
                };
            }
        } else {
            const first_kv = self.map.iterator().next();
            if (first_kv != null) {
                return Table.KV{
                    .key = first_kv.?.key,
                    .value = first_kv.?.value,
                };
            }
        }
        return null;
    }
};

test "arrayIndex" {
    const nil = Value.nil;
    var dummyObj = object.GCObject{};
    const str = Value{ .string = &dummyObj };
    const valid_num = Value{ .number = 10 };
    const invalid_num = Value{ .number = 5.5 };

    std.testing.expectError(error.InvalidArrayKey, Table.arrayIndex(nil));
    std.testing.expectError(error.InvalidArrayKey, Table.arrayIndex(str));
    std.testing.expectError(error.InvalidArrayKey, Table.arrayIndex(invalid_num));
    std.testing.expectEqual(@as(usize, 10), try Table.arrayIndex(valid_num));
}

test "init and initCapacity" {
    var tbl = Table.init(std.testing.allocator);
    defer tbl.deinit();

    var tblCapacity = try Table.initCapacity(std.testing.allocator, 5, 5);
    defer tblCapacity.deinit();
}

test "get" {
    var tbl = try Table.initCapacity(std.testing.allocator, 5, 5);
    defer tbl.deinit();

    // TODO: This is basically just a test of how its currently implemented, not
    //       necessarily how it *should* work.

    // because we pre-allocated the array portion, this will give us nil
    // instead of null
    std.testing.expectEqual(Value.nil, tbl.get(Value{ .number = 1 }).?.*);

    // hash map preallocation will still give us null though
    var dummyObj = object.GCObject{};
    std.testing.expect(null == tbl.get(Value{ .string = &dummyObj }));
}

test "getn" {
    var tbl = try Table.initCapacity(std.testing.allocator, 5, 5);
    defer tbl.deinit();

    std.testing.expectEqual(@as(usize, 0), tbl.getn());
    const key = Value{ .number = 1 };

    const val = try tbl.getOrCreate(key);
    std.testing.expectEqual(@as(usize, 0), tbl.getn());

    val.* = Value{ .number = 1 };
    std.testing.expectEqual(@as(usize, 1), tbl.getn());

    val.* = Value.nil;
    std.testing.expectEqual(@as(usize, 0), tbl.getn());
}

test "getn quirk 1" {
    //   tbl = {1,2,3,4,5,6}
    var tbl = try Table.initCapacity(std.testing.allocator, 6, 0);
    defer tbl.deinit();

    var i: usize = 1;
    while (i <= 6) : (i += 1) {
        const key = Value{ .number = @intToFloat(f64, i) };
        const val = tbl.get(key).?;
        val.* = key;
    }
    //   assert(#tbl == 6)
    std.testing.expectEqual(@as(usize, 6), tbl.getn());

    //   tbl[3] = nil
    const val3 = tbl.get(Value{ .number = 3 }).?;
    val3.* = Value.nil;
    //   assert(#tbl == 6)
    std.testing.expectEqual(@as(usize, 6), tbl.getn());

    //   tbl[6] = nil
    const val6 = tbl.get(Value{ .number = 6 }).?;
    val6.* = Value.nil;
    //   assert(#tbl == 2)
    std.testing.expectEqual(@as(usize, 2), tbl.getn());
}

test "getn quirk 2" {
    // For some reason, {1,2,nil,4,5} seems to put the first 4 elements in
    // the array, and the '5' in the hash map.
    // TODO: Why does this work this way in Lua?
    //   tbl = {1,2,nil,4,5}
    var tbl = try Table.initCapacity(std.testing.allocator, 4, 0);
    defer tbl.deinit();

    var i: usize = 1;
    while (i <= 5) : (i += 1) {
        const key = Value{ .number = @intToFloat(f64, i) };
        const val = try tbl.getOrCreate(key);
        val.* = if (i != 3) key else Value.nil;
    }
    //   assert(#tbl == 5)
    std.testing.expectEqual(@as(usize, 5), tbl.getn());

    //   tbl[10] = 10
    const val10 = try tbl.getOrCreate(Value{ .number = 10 });
    val10.* = Value{ .number = 10 };
    //   assert(#tbl == 10)
    std.testing.expectEqual(@as(usize, 10), tbl.getn());

    //   tbl[20] = 20
    const val20 = try tbl.getOrCreate(Value{ .number = 20 });
    val20.* = Value{ .number = 20 };
    //   assert(#tbl == 20)
    std.testing.expectEqual(@as(usize, 20), tbl.getn());

    //   i = 40
    //   while i<2147483647 do
    //     tbl[i] = i
    //     i = i*2
    //   end
    i = 40;
    while (i < std.math.maxInt(i32)) : (i *= 2) {
        const val = try tbl.getOrCreate(Value{ .number = @intToFloat(f64, i) });
        val.* = Value{ .number = @intToFloat(f64, i) };
    }
    //   assert(#tbl == 2)
    std.testing.expectEqual(@as(usize, 2), tbl.getn());
}

test "getn quirk 2, next iterator" {
    var tbl = try Table.initCapacity(std.testing.allocator, 4, 0);
    defer tbl.deinit();

    var i: usize = 1;
    while (i <= 5) : (i += 1) {
        const key = Value{ .number = @intToFloat(f64, i) };
        const val = try tbl.getOrCreate(key);
        val.* = if (i != 3) key else Value.nil;
    }
    // 1-5 except 3
    var expected_iterations: usize = 4;

    i = 10;
    while (i < std.math.maxInt(i32)) : (i *= 2) {
        const val = try tbl.getOrCreate(Value{ .number = @intToFloat(f64, i) });
        val.* = Value{ .number = @intToFloat(f64, i) };
        expected_iterations += 1;
    }

    var iterations: usize = 0;
    var kv = Table.KV{ .key = Value.nil, .value = Value.nil };
    while (tbl.next(kv.key)) |next_kv| {
        iterations += 1;
        kv = next_kv;
    }
    std.testing.expectEqual(expected_iterations, iterations);
}
