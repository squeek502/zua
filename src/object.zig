const std = @import("std");

// This is a placeholder implementation to be filled in
// as needed

pub const Type = enum {
    None,
    Nil,
    Boolean,
    LightUserdata,
    Number,
    String,
    Table,
    Function,
    Userdata,
    Thread,

    pub fn isCollectable(self: Type) bool {
        switch (self) {
            .String, .Table, .Function, .Userdata, .Thread => return true,
            else => return false,
        }
    }
};

pub const GCObject = struct {};

pub const Value = union(Type) {
    None: void,
    Nil: void,
    Boolean: bool,
    LightUserdata: *c_void, // TODO: what type should this be?
    Number: f64,
    String: *GCObject,
    Table: *GCObject,
    Function: *GCObject,
    Userdata: *GCObject,
    Thread: *GCObject,

    pub fn getType(self: Value) Type {
        return @as(Type, self);
    }

    pub fn isCollectable(self: Value) bool {
        return self.getType().isCollectable();
    }
};

test "collectability" {
    const nil = Value.Nil;
    var dummyObj = GCObject{};
    const str = Value{ .String = &dummyObj };
    std.testing.expect(!nil.isCollectable());
    std.testing.expect(str.isCollectable());
}
