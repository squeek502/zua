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
    maxstacksize: u8,
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
