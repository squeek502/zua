const std = @import("std");

pub const lex = @import("lex.zig");
pub const parse = @import("parse.zig");
pub const parse_literal = @import("parse_literal.zig");
pub const object = @import("object.zig");
pub const table = @import("table.zig");
pub const opcodes = @import("opcodes.zig");
pub const dump = @import("dump.zig");
pub const compiler = @import("compiler.zig");
pub const ast = @import("ast.zig");
pub const debug = @import("debug.zig");

pub fn main() void {}

test "zua" {
    std.testing.refAllDecls(@This());
}
