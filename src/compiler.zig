const std = @import("std");
const Allocator = std.mem.Allocator;
const zua = @import("zua.zig");
const Instruction = zua.opcodes.Instruction;
const Node = zua.ast.Node;
const Function = zua.object.Function;
const Constant = zua.object.Constant;
const Lexer = zua.lex.Lexer;
const Parser = zua.parse.Parser;

pub fn compile(allocator: *Allocator, source: []const u8) !Function {
    var lexer = Lexer.init(source, source);
    var parser = Parser.init(allocator, &lexer);
    var tree = try parser.parse();
    defer tree.deinit();

    var arena_allocator = std.heap.ArenaAllocator.init(allocator);
    defer arena_allocator.deinit();
    const arena = &arena_allocator.allocator;

    var compiler = Compiler{
        .source = source,
        .arena = arena,
        .allocator = allocator,
        .code = std.ArrayList(Instruction).init(allocator),
        .constants = std.ArrayList(Constant).init(allocator),
        .constants_map = Constant.Map.init(allocator),
    };
    defer compiler.deinit();

    try compiler.genNode(tree.node);

    return Function{
        .name = "",
        .code = compiler.code.toOwnedSlice(),
        .constants = compiler.constants.toOwnedSlice(),
        .allocator = allocator,
        .maxstacksize = 3,
    };
}

pub const Compiler = struct {
    source: []const u8,
    arena: *Allocator,
    allocator: *Allocator,
    code: std.ArrayList(Instruction),
    constants: std.ArrayList(Constant),
    constants_map: Constant.Map,
    // TODO: actual implementation
    free_register: u8 = 1,

    pub const Error = error{CompileError} || Allocator.Error;

    pub fn deinit(self: *Compiler) void {
        self.code.deinit();
        self.constants.deinit();
        self.constants_map.deinit();
    }

    pub fn genNode(self: *Compiler, node: *Node) Error!void {
        switch (node.id) {
            .chunk => try self.genChunk(@fieldParentPtr(Node.Chunk, "base", node)),
            .call => try self.genCall(@fieldParentPtr(Node.Call, "base", node)),
            .literal => try self.genLiteral(@fieldParentPtr(Node.Literal, "base", node)),
            .identifier => try self.genIdentifier(@fieldParentPtr(Node.Identifier, "base", node)),
            else => unreachable, // TODO
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
                .B = @intCast(u9, 1 + call.arguments.len), // fn + num args
                .C = 1,
            },
        });
    }

    pub fn genLiteral(self: *Compiler, literal: *Node.Literal) Error!void {
        switch (literal.token.id) {
            .string => {
                const string_source = self.source[literal.token.start..literal.token.end];
                var buf = try self.allocator.alloc(u8, string_source.len);
                defer self.allocator.free(buf);
                const parsed = zua.parse_literal.parseString(string_source, buf);
                const index = try self.putConstant(Constant{ .string = parsed });
                try self.code.append(.{
                    .iABx = .{
                        .op = .loadk,
                        // TODO: these are probably wrong, they just happen to line up for the hello world
                        .A = @intCast(u8, index),
                        .Bx = @intCast(u18, index),
                    },
                });
            },
            .number => {
                const number_source = self.source[literal.token.start..literal.token.end];
                const parsed = zua.parse_literal.parseNumber(number_source);
                const index = try self.putConstant(Constant{ .number = parsed });
                try self.code.append(.{
                    .iABx = .{
                        .op = .loadk,
                        // TODO: these are probably wrong, they just happen to line up for the hello world
                        .A = @intCast(u8, index),
                        .Bx = @intCast(u18, index),
                    },
                });
            },
            .keyword_true, .keyword_false => {
                const val = literal.token.id == .keyword_true;
                try self.code.append(.{
                    .iABC = .{
                        .op = .loadbool,
                        .A = self.free_register,
                        .B = @boolToInt(val),
                        .C = 0,
                    },
                });
                self.free_register += 1;
            },
            .keyword_nil => {
                try self.code.append(.{
                    .iABC = .{
                        .op = .loadnil,
                        .A = self.free_register,
                        .B = 1,
                        .C = 0,
                    },
                });
                self.free_register += 1;
            },
            else => unreachable,
        }
    }

    pub fn genIdentifier(self: *Compiler, node: *Node.Identifier) Error!void {
        const name = self.source[node.token.start..node.token.end];
        const index = try self.putConstant(Constant{ .string = name });
        try self.code.append(.{
            .iABx = .{
                .op = .getglobal,
                // TODO: these are probably wrong, they just happen to line up for the hello world
                .A = @intCast(u8, index),
                .Bx = @intCast(u18, index),
            },
        });
    }

    pub fn putConstant(self: *Compiler, constant: Constant) Error!usize {
        const result = try self.constants_map.getOrPut(constant);
        if (result.found_existing) {
            return result.entry.value;
        } else {
            result.entry.value = self.constants.items.len;
            var final_constant = constant;
            if (constant == .string) {
                // dupe the string so that the resulting Function owns all the memory
                const dupe = try self.allocator.dupe(u8, constant.string);
                final_constant = Constant{ .string = dupe };
            }
            try self.constants.append(final_constant);
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

test "compile print number literal" {
    var chunk = try compile(std.testing.allocator, "print(nil, true)");
    defer chunk.deinit();

    var buf = std.ArrayList(u8).init(std.testing.allocator);
    defer buf.deinit();
    try zua.dump.write(chunk, buf.writer());

    //try std.fs.cwd().writeFile("zuac.out", buf.items);
}
