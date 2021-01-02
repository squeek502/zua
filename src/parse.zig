const std = @import("std");
const Allocator = std.mem.Allocator;
const Lexer = @import("lex.zig").Lexer;
const Token = @import("lex.zig").Token;
const Node = @import("ast.zig").Node;
const Tree = @import("ast.zig").Tree;

// Notes:
//
// Lua parser always parses into a function (called the 'main' function) which
// is always varargs (the values in the varargs differs depending on Lua version)
//
// Lua parses directly into bytecode with no AST step in between. This implementation
// will generate an AST, though, so it won't be a 1:1 port of the Lua parser.
//
// The functions in Parser are currently named the same as their Lua C
// counterparts, but that will/should probably change.

pub fn parse(allocator: *Allocator, source: []const u8) !*Tree {
    var lexer = Lexer.init(source);
    const first_token = try lexer.next();

    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();

    var parser = Parser{
        .lexer = &lexer,
        .token = first_token,
        .allocator = allocator,
        .arena = &arena.allocator,
    };
    const chunk = try parser.chunk();

    const tree = try parser.arena.create(Tree);
    tree.* = .{
        .node = chunk,
        .source = source,
        .arena = arena.state,
        .allocator = allocator,
    };
    return tree;
}

pub const Parser = struct {
    const Self = @This();

    lexer: *Lexer,
    token: Token,
    allocator: *Allocator,
    arena: *Allocator,

    /// chunk -> { stat [`;'] }
    fn chunk(self: *Self) !*Node {
        var statements = std.ArrayList(*Node).init(self.allocator);
        defer statements.deinit();

        // TODO: islast
        // TODO: levels
        while (!block_follow(self.token)) {
            try statements.append(try self.statement());
            _ = try self.testcharnext(';');
        }

        const node = try self.arena.create(Node.Chunk);
        node.* = .{
            .body = try self.arena.dupe(*Node, statements.items),
        };
        return &node.base;
    }

    fn statement(self: *Self) !*Node {
        // TODO: other statements
        return self.exprstat();
    }

    fn exprstat(self: *Self) !*Node {
        return self.primaryexp();
    }

    fn primaryexp(self: *Self) !*Node {
        var arguments = std.ArrayList(*Node).init(self.allocator);
        defer arguments.deinit();

        var prefix = try self.prefixexp();
        while (true) {
            switch (self.token.id) {
                .single_char => unreachable, // TODO
                .string => {
                    const node = try self.arena.create(Node.StringLiteral);
                    node.* = .{
                        .token = self.token,
                    };
                    try arguments.append(&node.base);
                    self.token = try self.lexer.next();
                },
                .eof => break,
                else => unreachable,
            }
        }

        var call = try self.arena.create(Node.Call);
        call.* = .{
            .expression = prefix,
            .arguments = try self.arena.dupe(*Node, arguments.items),
        };
        return &call.base;
    }

    fn prefixexp(self: *Self) !*Node {
        switch (self.token.id) {
            .name => {
                const node = try self.arena.create(Node.Identifier);
                node.* = .{
                    .token = self.token,
                };
                self.token = try self.lexer.next();
                return &node.base;
            },
            else => unreachable, // TODO
        }
    }

    // TODO: move to Token?
    fn block_follow(token: Token) bool {
        return switch (token.id) {
            .keyword_else,
            .keyword_elseif,
            .keyword_end,
            .keyword_until,
            .eof,
            => true,
            else => false,
        };
    }

    /// Skip to next token if the current token matches the given ID
    fn testnext(self: *Self, id: Token.Id) !bool {
        if (self.token.id == id) {
            self.token = try self.lexer.next();
            return true;
        }
        return false;
    }

    /// Skip to next token if the current token is a single character token with the given value
    fn testcharnext(self: *Self, char: u8) !bool {
        if (self.token.id == .single_char and self.token.char.? == char) {
            self.token = try self.lexer.next();
            return true;
        }
        return false;
    }
};

test "parse hello world" {
    const allocator = std.testing.allocator;
    var tree = try parse(allocator, "print \"hello world\"");
    defer tree.deinit();

    var buf = std.ArrayList(u8).init(allocator);
    defer buf.deinit();

    try tree.dump(buf.writer());
    std.testing.expectEqualStrings(buf.items,
        \\chunk
        \\ call
        \\  identifier
        \\  (
        \\   string_literal
        \\  )
        \\
    );

    std.testing.expectEqual(Node.Id.chunk, tree.node.id);
    const chunk = tree.node.cast(.chunk).?;
    std.testing.expectEqual(@as(usize, 1), chunk.body.len);
    std.testing.expectEqual(Node.Id.call, chunk.body[0].id);
    const call = chunk.body[0].cast(.call).?;
    std.testing.expectEqual(Node.Id.identifier, call.expression.id);
    std.testing.expectEqual(@as(usize, 1), call.arguments.len);
    std.testing.expectEqual(Node.Id.string_literal, call.arguments[0].id);
}
