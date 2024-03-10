const builtin = @import("builtin");
const std = @import("std");
const opcodes = @import("opcodes.zig");
const Instruction = opcodes.Instruction;
const object = @import("object.zig");
const Function = object.Function;
const Constant = object.Constant;
const native_endian = builtin.cpu.arch.endian();

pub const signature = "\x1BLua";
pub const luac_version: u8 = 0x51;
pub const luac_format: u8 = 0;
pub const luac_headersize = 12;

// TODO make this actually optional, once we are actually capable of outputting debug info
const strip_debug_info = true;

pub fn write(chunk: Function, writer: anytype) @TypeOf(writer).Error!void {
    try writeHeader(writer);
    try writeFunction(chunk, writer);
}

pub fn writeHeader(writer: anytype) @TypeOf(writer).Error!void {
    try writer.writeAll(signature);
    try writer.writeByte(luac_version);
    try writer.writeByte(luac_format);
    try writer.writeByte(@intFromBool(builtin.target.cpu.arch.endian() == .little));
    try writer.writeByte(@sizeOf(c_int));
    try writer.writeByte(@sizeOf(usize));
    try writer.writeByte(@sizeOf(opcodes.Instruction));
    try writer.writeByte(@sizeOf(f64)); // sizeof(lua_Number)
    try writer.writeByte(@intFromBool(false)); // is lua_Number an integer type?
}

pub fn writeFunction(function: Function, writer: anytype) @TypeOf(writer).Error!void {
    // source info
    const chunk_name: ?[]const u8 = if (strip_debug_info) null else function.name;
    try writeString(chunk_name, writer);
    try writer.writeInt(c_int, 0, native_endian); // TODO: line defined
    try writer.writeInt(c_int, 0, native_endian); // TODO: last line defined
    try writer.writeByte(function.num_upvalues);
    try writer.writeByte(function.num_params);
    try writer.writeByte(function.varargs.dump());
    try writer.writeByte(function.max_stack_size);

    // instructions
    try writer.writeInt(c_int, @intCast(function.code.len), native_endian);
    try writer.writeAll(std.mem.sliceAsBytes(function.code));

    // constants
    // number of constants
    try writer.writeInt(c_int, @intCast(function.constants.len), native_endian);
    // each constant is dumped as a byte for its type followed by a dump of the value
    for (function.constants) |constant| {
        switch (constant) {
            .string => |string_literal| {
                try writer.writeByte(object.Value.Type.string.bytecodeId());
                try writeString(string_literal, writer);
            },
            .number => |number_literal| {
                try writer.writeByte(object.Value.Type.number.bytecodeId());
                try writer.writeAll(std.mem.asBytes(&number_literal));
            },
            .nil => {
                try writer.writeByte(object.Value.Type.nil.bytecodeId());
            },
            .boolean => |val| {
                try writer.writeByte(object.Value.Type.boolean.bytecodeId());
                try writer.writeByte(@intFromBool(val));
            },
        }
    }
    // number of functions
    try writer.writeInt(c_int, 0, native_endian);
    // TODO: functions

    // debug
    try writer.writeInt(c_int, 0, native_endian); // TODO: sizelineinfo
    // TODO: lineinfo
    try writer.writeInt(c_int, 0, native_endian); // TODO: sizelocvars
    // TODO: locvars
    try writer.writeInt(c_int, 0, native_endian); // TODO: sizeupvalues
    // TODO: upvalues
}

pub fn writeString(string: ?[]const u8, writer: anytype) @TypeOf(writer).Error!void {
    if (string == null) {
        try writer.writeInt(usize, 0, native_endian);
    } else {
        try writer.writeInt(usize, string.?.len + 1, native_endian);
        try writer.writeAll(string.?);
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

    const chunk = Function{
        .name = "",
        .code = &[_]Instruction{
            @bitCast(Instruction.ABC.init(.@"return", 0, 1, 0)),
        },
        .constants = &[_]Constant{},
        .max_stack_size = 0,
    };

    try write(chunk, buf.writer());
}

test "hello world" {
    var buf = std.ArrayList(u8).init(std.testing.allocator);
    defer buf.deinit();

    const chunk = Function{
        .allocator = null,
        .name = "",
        .code = &[_]Instruction{
            @bitCast(Instruction.ABx.init(.getglobal, 0, 0)),
            @bitCast(Instruction.ABx.init(.loadk, 1, 1)),
            @bitCast(Instruction.ABC.init(.call, 0, 2, 1)),
            @bitCast(Instruction.ABC.init(.@"return", 0, 1, 0)),
        },
        .constants = &[_]Constant{
            Constant{ .string = "print" },
            Constant{ .string = "hello world" },
        },
        .max_stack_size = 2,
    };

    try write(chunk, buf.writer());
}

test "constants" {
    var buf = std.ArrayList(u8).init(std.testing.allocator);
    defer buf.deinit();

    const chunk = Function{
        .allocator = null,
        .name = "",
        .code = &[_]Instruction{
            @bitCast(Instruction.ABC.init(.@"return", 0, 1, 0)),
        },
        .constants = &[_]Constant{
            Constant{ .string = "print" },
            Constant{ .string = "hello world" },
            Constant{ .boolean = true },
            Constant{ .boolean = false },
            Constant.nil,
            Constant{ .number = 123 },
        },
        .max_stack_size = 0,
    };

    try write(chunk, buf.writer());
    //std.debug.print("{e}\n", .{buf.items});
}
