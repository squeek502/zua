const std = @import("std");
const Allocator = std.mem.Allocator;
const zua = @import("zua.zig");
const Instruction = zua.opcodes.Instruction;
const Node = zua.ast.Node;

pub const Chunk = struct {
    name: []const u8,
    code: []const Instruction,
    strings: []const []const u8,
    allocator: ?*Allocator = null,

    pub fn deinit(self: *Chunk) void {
        if (self.allocator == null) return;
        // we own all of the strings memory, so free each one
        for (self.strings) |string| {
            self.allocator.?.free(string);
        }
        self.allocator.?.free(self.strings);
        self.allocator.?.free(self.code);
    }
};

pub fn compile(allocator: *Allocator, source: []const u8) !Chunk {
    var tree = try zua.parse.parse(allocator, source);
    defer tree.deinit();

    var arena_allocator = std.heap.ArenaAllocator.init(allocator);
    defer arena_allocator.deinit();
    const arena = &arena_allocator.allocator;

    var compiler = Compiler{
        .source = source,
        .arena = arena,
        .allocator = allocator,
        .code = std.ArrayList(Instruction).init(allocator),
        .strings = std.ArrayList([]const u8).init(allocator),
        .string_map = std.StringHashMap(usize).init(allocator),
    };
    defer compiler.deinit();

    try compiler.genNode(tree.node);

    return Chunk{
        .name = "",
        .code = compiler.code.toOwnedSlice(),
        .strings = compiler.strings.toOwnedSlice(),
        .allocator = allocator,
    };
}

pub const Compiler = struct {
    source: []const u8,
    arena: *Allocator,
    allocator: *Allocator,
    code: std.ArrayList(Instruction),
    strings: std.ArrayList([]const u8),
    string_map: std.StringHashMap(usize),

    pub const Error = error{CompileError} || Allocator.Error;

    pub fn deinit(self: *Compiler) void {
        self.code.deinit();
        self.strings.deinit();
        self.string_map.deinit();
    }

    pub fn genNode(self: *Compiler, node: *Node) Error!void {
        switch (node.id) {
            .chunk => try self.genChunk(@fieldParentPtr(Node.Chunk, "base", node)),
            .call => try self.genCall(@fieldParentPtr(Node.Call, "base", node)),
            .string_literal => try self.genStringLiteral(@fieldParentPtr(Node.StringLiteral, "base", node)),
            .identifier => try self.genIdentifier(@fieldParentPtr(Node.Identifier, "base", node)),
        }
    }

    pub fn genChunk(self: *Compiler, chunk: *Node.Chunk) Error!void {
        for (chunk.body) |node| {
            try self.genNode(node);
        }
        try self.code.append(.{
            .iABC = .{
                .op = .@"return",
                // TODO: what should these actually be?
                .A = 0,
                .B = 1,
                .C = 0,
            },
        });
    }

    pub fn genCall(self: *Compiler, call: *Node.Call) Error!void {
        try self.genNode(call.expression);
        for (call.arguments) |argument_node| {
            try self.genNode(argument_node);
        }
        try self.code.append(.{
            .iABC = .{
                .op = .call,
                // TODO: what should these actually be?
                .A = 0,
                .B = 2,
                .C = 1,
            },
        });
    }

    pub fn genStringLiteral(self: *Compiler, node: *Node.StringLiteral) Error!void {
        const string_source = self.source[node.token.start..node.token.end];
        var buf = try self.allocator.alloc(u8, string_source.len);
        defer self.allocator.free(buf);
        const parsed = zua.parse_literal.parseString(string_source, buf);
        const index = try self.putString(parsed);
        try self.code.append(.{
            .iABx = .{
                .op = .loadk,
                // TODO: these are probably wrong, they just happen to line up for the hello world
                .A = @intCast(u8, index),
                .Bx = @intCast(u18, index),
            },
        });
    }

    pub fn genIdentifier(self: *Compiler, node: *Node.Identifier) Error!void {
        const name = self.source[node.token.start..node.token.end];
        const index = try self.putString(name);
        try self.code.append(.{
            .iABx = .{
                .op = .getglobal,
                // TODO: these are probably wrong, they just happen to line up for the hello world
                .A = @intCast(u8, index),
                .Bx = @intCast(u18, index),
            },
        });
    }

    pub fn putString(self: *Compiler, str: []const u8) Error!usize {
        const result = try self.string_map.getOrPut(str);
        if (result.found_existing) {
            return result.entry.value;
        } else {
            result.entry.value = self.strings.items.len;
            // dupe so that the resulting Chunk owns all the memory
            const dupe = try self.allocator.dupe(u8, str);
            try self.strings.append(dupe);
            return result.entry.value;
        }
    }
};

test "compile hello world" {
    var chunk = try compile(std.testing.allocator, "print \"hello world\"");
    defer chunk.deinit();

    var buf = std.ArrayList(u8).init(std.testing.allocator);
    defer buf.deinit();
    try zua.dump.write(chunk, buf.writer());

    try std.fs.cwd().writeFile("zuac.out", buf.items);
}
