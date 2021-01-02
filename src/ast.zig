const std = @import("std");
const Allocator = std.mem.Allocator;
const Token = @import("lex.zig").Token;

pub const Tree = struct {
    node: *Node,

    /// not owned by the tree
    source: []const u8,

    arena: std.heap.ArenaAllocator.State,
    allocator: *Allocator,

    pub fn deinit(self: *Tree) void {
        self.arena.promote(self.allocator).deinit();
    }

    pub fn dump(self: *Tree, writer: anytype) @TypeOf(writer).Error!void {
        try self.node.dump(writer, 0);
    }
};

pub const Node = struct {
    id: Id,

    pub const Id = enum {
        chunk,
        call,
        string_literal,
        identifier,

        pub fn Type(id: Id) type {
            return switch (id) {
                .chunk => Chunk,
                .call => Call,
                .string_literal => StringLiteral,
                .identifier => Identifier,
            };
        }
    };

    pub fn cast(base: *Node, comptime id: Id) ?*id.Type() {
        if (base.id == id) {
            return @fieldParentPtr(id.Type(), "base", base);
        }
        return null;
    }

    pub const Chunk = struct {
        base: Node = .{ .id = .chunk },
        body: []*Node,
    };

    pub const Call = struct {
        base: Node = .{ .id = .call },
        expression: *Node,
        arguments: []*Node,
    };

    pub const StringLiteral = struct {
        base: Node = .{ .id = .string_literal },
        token: Token,
    };

    pub const Identifier = struct {
        base: Node = .{ .id = .identifier },
        token: Token,
    };

    pub fn dump(
        node: *const Node,
        writer: anytype,
        indent: usize,
    ) @TypeOf(writer).Error!void {
        try writer.writeByteNTimes(' ', indent);
        try writer.writeAll(@tagName(node.id));
        try writer.writeAll("\n");
        switch (node.id) {
            .chunk => {
                const chunk = @fieldParentPtr(Node.Chunk, "base", node);
                for (chunk.body) |body_node| {
                    try body_node.dump(writer, indent + 1);
                }
            },
            .call => {
                const call = @fieldParentPtr(Node.Call, "base", node);
                try call.expression.dump(writer, indent + 1);
                try writer.writeByteNTimes(' ', indent + 1);
                try writer.writeAll("(\n");
                for (call.arguments) |arg_node| {
                    try arg_node.dump(writer, indent + 2);
                }
                try writer.writeByteNTimes(' ', indent + 1);
                try writer.writeAll(")\n");
            },
            .identifier => {},
            .string_literal => {},
        }
    }
};
