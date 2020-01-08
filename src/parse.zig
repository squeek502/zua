const std = @import("std");

// Notes:
//
// Lua parser always parses into a function (called the 'main' function) which
// is always varargs (the values in the varargs differs depending on Lua version)

pub const Parser = struct {};
