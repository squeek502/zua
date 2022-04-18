//! A library interface to luaU_dump
//! Intended to be useful for testing against the bytecode output of luac

const c = @cImport({
    @cInclude("lauxlib.h");
    @cInclude("lua.h");
    @cInclude("lundump.h");
    @cInclude("lstate.h");
});
const std = @import("std");
const Allocator = std.mem.Allocator;

// need a non-opaque struct definition to get at the `top` field
const lua_State = extern struct {
    // common header
    next: ?*anyopaque,
    tt: u8,
    marked: u8,
    // lua_State
    status: u8,
    top: c.StkId,
    // rest is irrelevant for our purposes
};

pub fn loadAndDumpAlloc(allocator: Allocator, chunk: [:0]const u8) ![]const u8 {
    _ = allocator;
    _ = chunk;

    var L: *c.lua_State = open: {
        break :open c.lua_open() orelse return error.OutOfMemory;
    };
    defer c.lua_close(L);

    const load_result = c.luaL_loadstring(L, chunk.ptr);
    switch (load_result) {
        0 => {},
        c.LUA_ERRSYNTAX => return error.SyntaxError,
        c.LUA_ERRMEM => return error.OutOfMemory,
        else => unreachable,
    }
    std.debug.assert(c.lua_type(L, -1) == c.LUA_TFUNCTION);

    // get the proto (this should be equivalent to `toproto(L,-1)` in luac.c)
    const concrete_state = @ptrCast(*lua_State, @alignCast(8, L));
    const first_free_slot_in_stack = concrete_state.top;
    const top_of_stack: *c.lua_TValue = first_free_slot_in_stack - 1;
    const gc: *c.GCObject = top_of_stack.value.gc;
    const proto = gc.cl.l.p;

    var output = std.ArrayList(u8).init(allocator);
    errdefer output.deinit();

    const strip = true;
    const dump_result = c.luaU_dump(
        L,
        proto,
        luaWriterArrayList,
        &output,
        if (strip) 1 else 0,
    );
    if (dump_result != 0) {
        return error.DumpError;
    }

    return output.toOwnedSlice();
}

fn luaWriterArrayList(
    L: ?*c.lua_State,
    p: ?*const anyopaque,
    sz: usize,
    ud: ?*anyopaque,
) callconv(.C) c_int {
    _ = L;
    var array_list = @ptrCast(*std.ArrayList(u8), @alignCast(@alignOf(*std.ArrayList(u8)), ud.?));
    var slice = @ptrCast([*]const u8, p.?)[0..sz];
    array_list.appendSlice(slice) catch return 1;
    return 0;
}

test {
    const allocator = std.testing.allocator;

    const bytecode = try loadAndDumpAlloc(allocator, "print \"hello world\"");
    defer allocator.free(bytecode);

    const expected_bytecode = "\x1bLuaQ\x00\x01\x04\x08\x04\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x02\x04\x00\x00\x00\x05\x00\x00\x00A@\x00\x00\x1c@\x00\x01\x1e\x00\x80\x00\x02\x00\x00\x00\x04\x06\x00\x00\x00\x00\x00\x00\x00print\x00\x04\x0c\x00\x00\x00\x00\x00\x00\x00hello world\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00";
    try std.testing.expectEqualSlices(u8, expected_bytecode, bytecode);
}
