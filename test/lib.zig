//! Library of helpers used in test code

const std = @import("std");

pub const luac = @import("luac.zig");

test {
    _ = std.testing.refAllDecls(@This());
}
