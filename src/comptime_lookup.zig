const std = @import("std");

pub fn AutoComptimeLookup(comptime K: type, comptime V: type, comptime kvs: anytype) type {
    return ComptimeLookup(K, V, std.hash_map.getAutoEqlFn(K, void), kvs);
}

/// The most basic comptime map possible, should not be used for anything intended to be performant
/// (a regular ol' linear search is performed for each lookup)
pub fn ComptimeLookup(comptime K: type, comptime V: type, comptime eql: fn (ctx: void, a: K, b: K) bool, comptime kvs: anytype) type {
    return struct {
        pub fn has(str: []const u8) bool {
            return get(str) != null;
        }

        pub fn get(lookup_key: K) ?V {
            inline for (kvs) |kv| {
                const key = kv.@"0";
                if (eql({}, lookup_key, key)) {
                    return if (V != void) kv.@"1" else {};
                }
            }
            return null;
        }
    };
}
