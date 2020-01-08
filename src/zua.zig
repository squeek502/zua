const std = @import("std");

pub const lex = @import("lex.zig");
pub const parse = @import("parse.zig");
pub const parse_literal = @import("parse_literal.zig");

pub fn main() void {
}

test "zua" {
    _ = @import("lex.zig");
    _ = @import("parse.zig");
    _ = @import("parse_literal.zig");
}
