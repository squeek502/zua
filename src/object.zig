const std = @import("std");

// This is a placeholder implementation to be filled in
// as needed

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
};

pub const GCObject = struct {};

pub const Value = union(Type) {
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
