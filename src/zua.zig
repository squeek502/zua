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
    _ = @import("lex.zig");
    _ = @import("parse.zig");
    _ = @import("parse_literal.zig");
    _ = @import("object.zig");
    _ = @import("table.zig");
    _ = @import("opcodes.zig");
    _ = @import("dump.zig");
    _ = @import("compiler.zig");
    _ = @import("ast.zig");
    _ = @import("debug.zig");
}
