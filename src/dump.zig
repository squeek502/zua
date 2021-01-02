const builtin = @import("builtin");
const std = @import("std");
const opcodes = @import("opcodes.zig");
const Instruction = opcodes.Instruction;
const object = @import("object.zig");
const Chunk = @import("compiler.zig").Chunk;

pub const signature = "\x1BLua";
pub const luac_version: u8 = 0x51;
pub const luac_format: u8 = 0;
pub const luac_headersize = 12;

pub fn write(chunk: Chunk, writer: anytype) @TypeOf(writer).Error!void {
    try writeHeader(writer);

    // source info
    try writeString(chunk.name, writer);
    try writer.writeIntNative(c_int, 0); // TODO: line defined
    try writer.writeIntNative(c_int, 0); // TODO: last line defined
    try writer.writeByte(0); // TODO: num up values
    try writer.writeByte(0); // TODO: num params
    try writer.writeByte(2); // TODO: is var arg (2 = VARARG_ISVARARG, see lobject.h)
    try writer.writeByte(2); // TODO: max stack size

    // instructions
    try writer.writeIntNative(c_int, @intCast(c_int, chunk.code.len));
    try writer.writeAll(std.mem.sliceAsBytes(chunk.code));

    // constants
    // number of constants
    // TODO: other types of constants (nil, bool, number)
    try writer.writeIntNative(c_int, @intCast(c_int, chunk.strings.len));
    for (chunk.strings) |string| {
        try writer.writeByte(object.Type.string.bytecodeId());
        try writeString(string, writer);
    }
    // number of functions
    // TODO: functions
    try writer.writeIntNative(c_int, 0);

    // debug
    try writer.writeIntNative(c_int, 0); // TODO: sizelineinfo
    // TODO: lineinfo
    try writer.writeIntNative(c_int, 0); // TODO: sizelocvars
    // TODO: locvars
    try writer.writeIntNative(c_int, 0); // TODO: sizeupvalues
    // TODO: upvalues
}

pub fn writeHeader(writer: anytype) @TypeOf(writer).Error!void {
    try writer.writeAll(signature);
    try writer.writeByte(luac_version);
    try writer.writeByte(luac_format);
    try writer.writeByte(@boolToInt(builtin.endian == .Little));
    try writer.writeByte(@sizeOf(c_int));
    try writer.writeByte(@sizeOf(usize));
    try writer.writeByte(@sizeOf(opcodes.Instruction));
    try writer.writeByte(@sizeOf(f64)); // sizeof(lua_Number)
    try writer.writeByte(@boolToInt(false)); // is lua_Number an integer type?
}

pub fn writeString(string: []const u8, writer: anytype) @TypeOf(writer).Error!void {
    if (string.len == 0) {
        try writer.writeIntNative(usize, 0);
    } else {
        try writer.writeIntNative(usize, string.len + 1);
        try writer.writeAll(string);
        try writer.writeByte(0);
    }
}

test "header" {
    var buf = std.ArrayList(u8).init(std.testing.allocator);
    defer buf.deinit();

    try writeHeader(buf.writer());
}

test "just return" {
    var buf = std.ArrayList(u8).init(std.testing.allocator);
    defer buf.deinit();

    var chunk = Chunk{
        .name = "",
        .code = &[_]Instruction{
            .{
                .iABC = .{
                    .op = .@"return",
                    .A = 0,
                    .B = 1,
                    .C = 0,
                },
            },
        },
        .strings = &[_][]const u8{},
    };

    try write(chunk, buf.writer());
}

test "hello world" {
    var buf = std.ArrayList(u8).init(std.testing.allocator);
    defer buf.deinit();

    var chunk = Chunk{
        .allocator = null,
        .name = "",
        .code = &[_]Instruction{
            .{
                .iABx = .{
                    .op = .getglobal,
                    .A = 0,
                    .Bx = 0,
                },
            },
            .{
                .iABx = .{
                    .op = .loadk,
                    .A = 1,
                    .Bx = 1,
                },
            },
            .{
                .iABC = .{
                    .op = .call,
                    .A = 0,
                    .B = 2,
                    .C = 1,
                },
            },
            .{
                .iABC = .{
                    .op = .@"return",
                    .A = 0,
                    .B = 1,
                    .C = 0,
                },
            },
        },
        .strings = &[_][]const u8{
            "print",
            "hello world",
        },
    };

    try write(chunk, buf.writer());
}
