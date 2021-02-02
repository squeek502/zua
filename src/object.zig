const std = @import("std");
const zua = @import("zua.zig");
const Instruction = zua.opcodes.Instruction;
const Allocator = std.mem.Allocator;

// This is a placeholder implementation to be filled in
// as needed
pub const GCObject = struct {};

pub const Value = union(Value.Type) {
    none: void,
    nil: void,
    boolean: bool,
    light_userdata: *c_void, // TODO: what type should this be?
    number: f64,
    string: *GCObject,
    table: *GCObject,
    function: *GCObject,
    userdata: *GCObject,
    thread: *GCObject,

    pub const Type = enum {
        none,
        nil,
        boolean,
        light_userdata,
        number,
        string,
        table,
        function,
        userdata,
        thread,

        pub fn isCollectable(self: Type) bool {
            switch (self) {
                .string, .table, .function, .userdata, .thread => return true,
                else => return false,
            }
        }

        /// ID to be used when reading/writing Lua bytecode
        pub fn bytecodeId(self: Type) u8 {
            return switch (self) {
                .none => unreachable, // none is not serializable
                .nil => 0,
                .boolean => 1,
                .light_userdata => 2,
                .number => 3,
                .string => 4,
                .table => 5,
                .function => 6,
                .userdata => 7,
                .thread => 8,
            };
        }
    };

    pub fn getType(self: Value) Type {
        return @as(Type, self);
    }

    pub fn isCollectable(self: Value) bool {
        return self.getType().isCollectable();
    }
};

test "collectability" {
    const nil = Value.nil;
    var dummyObj = GCObject{};
    const str = Value{ .string = &dummyObj };
    std.testing.expect(!nil.isCollectable());
    std.testing.expect(str.isCollectable());
}

/// Called Proto in Lua (lobject.h)
pub const Function = struct {
    name: []const u8,
    code: []const Instruction,
    constants: []const Constant,
    varargs: Function.VarArgs = .{},
    max_stack_size: u8,
    num_params: u8 = 0,
    num_upvalues: u8 = 0,
    allocator: ?*Allocator = null,

    pub const VarArgs = struct {
        has_arg: bool = false,
        is_var_arg: bool = false,
        needs_arg: bool = false,

        pub fn dump(self: *const Function.VarArgs) u8 {
            var dumped: u8 = 0;
            if (self.has_arg) dumped |= 1;
            if (self.is_var_arg) dumped |= 2;
            if (self.needs_arg) dumped |= 4;
            return dumped;
        }

        pub fn undump(dumped: u8) Function.VarArgs {
            return Function.VarArgs{
                .has_arg = @intToBool(dumped & 1),
                .is_var_arg = @intToBool(dumped & 2),
                .needs_arg = @intToBool(dumped & 4),
            };
        }
    };

    pub fn deinit(self: *Function) void {
        if (self.allocator == null) return;
        // we own all of the strings memory, so free each one
        for (self.constants) |constant| {
            switch (constant) {
                .string => |val| {
                    self.allocator.?.free(val);
                },
                else => {},
            }
        }
        self.allocator.?.free(self.constants);
        self.allocator.?.free(self.code);
    }

    pub fn printCode(self: *Function) void {
        // TODO this is an (incomplete) direct port of the function PrintFunction in print.c
        // It could be cleaned up a lot.
        for (self.code) |instruction, i| {
            const op = instruction.op;
            var a: i32 = instruction.a;
            var b: i32 = @bitCast(Instruction.ABC, instruction).b;
            var c: i32 = @bitCast(Instruction.ABC, instruction).c;
            var bx: i32 = @bitCast(Instruction.ABx, instruction).bx;
            var sbx: i32 = @bitCast(Instruction.AsBx, instruction).getSignedBx();
            std.debug.print("\t{d}\t", .{i + 1});
            std.debug.print("[-]\t", .{}); // TODO line number
            std.debug.print("{}\t", .{op});
            switch (op.getOpMode()) {
                .iABC => {
                    std.debug.print("{d}", .{a});
                    if (op.getBMode() != .NotUsed) {
                        var b_for_display: i32 = if (zua.opcodes.isConstant(@intCast(u9, b)))
                            (-1 - @intCast(i32, zua.opcodes.getConstantIndex(@intCast(u9, b))))
                        else
                            b;
                        std.debug.print(" {d}", .{b_for_display});
                    }
                    if (op.getCMode() != .NotUsed) {
                        var c_for_display: i32 = if (zua.opcodes.isConstant(@intCast(u9, c)))
                            (-1 - @intCast(i32, zua.opcodes.getConstantIndex(@intCast(u9, c))))
                        else
                            c;
                        std.debug.print(" {d}", .{c_for_display});
                    }
                },
                .iABx => {
                    if (op.getBMode() == .ConstantOrRegisterConstant) {
                        std.debug.print("{d} {d}", .{ a, -1 - bx });
                    } else {
                        std.debug.print("{d} {d}", .{ a, bx });
                    }
                },
                .iAsBx => {},
            }
            std.debug.print("\n", .{});
        }
    }
};

pub const Constant = union(Constant.Type) {
    string: []const u8,
    number: f64,
    boolean: bool,
    nil: void,

    pub const Type = enum {
        string,
        number,
        nil,
        boolean,
    };

    fn hash(constant: Constant) u64 {
        switch (constant) {
            .boolean => |val| {
                const autoHashFn = std.hash_map.getAutoHashFn(@TypeOf(val));
                return autoHashFn(val);
            },
            .number => |val| {
                const floatBits = @typeInfo(@TypeOf(val)).Float.bits;
                const hashType = std.meta.Int(.unsigned, floatBits);
                const autoHashFn = std.hash_map.getAutoHashFn(hashType);
                return autoHashFn(@bitCast(hashType, val));
            },
            .string => |val| {
                return std.hash_map.hashString(val);
            },
            .nil => {
                return 0;
            },
        }
    }

    fn eql(a: Constant, b: Constant) bool {
        if (@as(Constant.Type, a) != @as(Constant.Type, b)) {
            return false;
        }
        return switch (a) {
            .string => std.mem.eql(u8, a.string, b.string),
            .number => a.number == b.number,
            .boolean => a.boolean == b.boolean,
            .nil => true,
        };
    }

    pub const Map = std.HashMap(Constant, usize, Constant.hash, Constant.eql, std.hash_map.DefaultMaxLoadPercentage);
};

/// Turns a 'source' string into a chunk id for display in errors, etc.
/// The buffer must have at least enough capacity to hold [string "..."]
/// TODO: this was in lobject.c in Lua, but it's a pretty weird place for it
pub fn getChunkId(source: []const u8, buf: []u8) []u8 {
    const buf_end: usize = buf_end: {
        switch (source[0]) {
            '=' => {
                // remove first char, truncate to buf capacity if needed
                const source_for_display = source[1..std.math.min(buf.len + 1, source.len)];
                std.mem.copy(u8, buf, source_for_display);
                break :buf_end source_for_display.len;
            },
            '@' => {
                var source_for_display = source[1..]; // skip the @
                const ellipsis = "...";
                const max_truncated_len = buf.len - ellipsis.len;
                var buf_index: usize = 0;
                if (source_for_display.len > max_truncated_len) {
                    const source_start_index = source_for_display.len - max_truncated_len;
                    source_for_display = source_for_display[source_start_index..];
                    std.mem.copy(u8, buf, ellipsis);
                    buf_index += ellipsis.len;
                }
                std.mem.copy(u8, buf[buf_index..], source_for_display);
                break :buf_end buf_index + source_for_display.len;
            },
            else => {
                const prefix = "[string \"";
                const suffix = "\"]";
                const ellipsis = "...";
                const min_display_len = prefix.len + ellipsis.len + suffix.len;
                std.debug.assert(buf.len >= min_display_len);

                // truncate to first newline
                const first_newline_index = std.mem.indexOfAny(u8, source, "\r\n");
                var source_for_display: []const u8 = if (first_newline_index != null) source[0..first_newline_index.?] else source;

                // trim to fit in buffer if necessary
                const max_source_len = buf.len - min_display_len;
                const needed_truncation = source_for_display.len > max_source_len;
                if (needed_truncation) {
                    source_for_display.len = max_source_len;
                }

                var fbs = std.io.fixedBufferStream(buf);
                const writer = fbs.writer();
                writer.writeAll(prefix) catch unreachable;
                writer.writeAll(source_for_display) catch unreachable;
                if (needed_truncation) {
                    writer.writeAll(ellipsis) catch unreachable;
                }
                writer.writeAll(suffix) catch unreachable;
                break :buf_end fbs.getPos() catch unreachable;
            },
        }
    };
    return buf[0..buf_end];
}

test "getChunkId" {
    var buf: [50]u8 = undefined;
    std.testing.expectEqualStrings("something", getChunkId("=something", &buf));
    std.testing.expectEqualStrings(
        "something that is long enough to actually need tru",
        getChunkId("=something that is long enough to actually need truncation", &buf),
    );

    std.testing.expectEqualStrings("something", getChunkId("@something", &buf));
    std.testing.expectEqualStrings(
        ".../is/long/enough/to/actually/need/truncation.lua",
        getChunkId("@something/that/is/long/enough/to/actually/need/truncation.lua", &buf),
    );

    std.testing.expectEqualStrings("[string \"something\"]", getChunkId("something", &buf));
    std.testing.expectEqualStrings("[string \"some\"]", getChunkId("some\nthing", &buf));
    std.testing.expectEqualStrings("[string \"some\"]", getChunkId("some\rthing", &buf));
    std.testing.expectEqualStrings(
        "[string \"something that is long enough to act...\"]",
        getChunkId("something that is long enough to actually need truncation", &buf),
    );

    var min_buf: [14]u8 = undefined;
    std.testing.expectEqualStrings("[string \"...\"]", getChunkId("anything", &min_buf));
}
