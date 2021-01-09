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

pub const ParseError = error{
    ExpectedCloseParen,
    FunctionArgumentsExpected,
};

pub const Parser = struct {
    const Self = @This();

    lexer: *Lexer,
    token: Token,
    allocator: *Allocator,
    arena: *Allocator,

    pub const Error = ParseError || Lexer.Error || Allocator.Error;

    /// chunk -> { stat [`;'] }
    fn chunk(self: *Self) Error!*Node {
        var statements = std.ArrayList(*Node).init(self.allocator);
        defer statements.deinit();

        // TODO: islast
        // TODO: levels
        try self.block(&statements);

        const node = try self.arena.create(Node.Chunk);
        node.* = .{
            .body = try self.arena.dupe(*Node, statements.items),
        };
        return &node.base;
    }

    fn statement(self: *Self) Error!*Node {
        switch (self.token.id) {
            .keyword_if => return self.ifstat(),
            .keyword_local => {
                self.token = try self.lexer.next();
                if (try self.testnext(.keyword_function)) {
                    return self.localfunc();
                } else {
                    return self.localstat();
                }
            },
            .keyword_while,
            .keyword_do,
            .keyword_for,
            .keyword_repeat,
            .keyword_function,
            .keyword_return,
            .keyword_break,
            => unreachable,
            else => return self.exprstat(),
        }
    }

    fn block(self: *Self, list: *std.ArrayList(*Node)) Error!void {
        while (!block_follow(self.token)) {
            try list.append(try self.statement());
            _ = try self.testcharnext(';');
        }
    }

    /// sort of the equivalent of Lua's test_then_block in lparser.c but handles else too
    fn ifclause(self: *Self) Error!*Node {
        const if_token = self.token;
        var condition: ?*Node = null;

        self.token = try self.lexer.next();
        switch (if_token.id) {
            .keyword_if, .keyword_elseif => {
                condition = try self.expr();

                std.debug.assert(self.token.id == .keyword_then); // TODO checknext
                self.token = try self.lexer.next();
            },
            .keyword_else => {},
            else => unreachable,
        }

        var body = std.ArrayList(*Node).init(self.allocator);
        defer body.deinit();

        try self.block(&body);

        var if_clause = try self.arena.create(Node.IfClause);
        if_clause.* = .{
            .if_token = if_token,
            .condition = condition,
            .body = try self.arena.dupe(*Node, body.items),
        };
        return &if_clause.base;
    }

    /// ifstat -> IF cond THEN block {ELSEIF cond THEN block} [ELSE block] END
    fn ifstat(self: *Self) Error!*Node {
        std.debug.assert(self.token.id == .keyword_if);

        var clauses = std.ArrayList(*Node).init(self.allocator);
        defer clauses.deinit();

        // if
        const if_clause = try self.ifclause();
        try clauses.append(if_clause);

        // elseif
        while (self.token.id == .keyword_elseif) {
            const elseif_clause = try self.ifclause();
            try clauses.append(elseif_clause);
        }

        // else
        if (self.token.id == .keyword_else) {
            const else_clause = try self.ifclause();
            try clauses.append(else_clause);
        }

        std.debug.assert(self.token.id == .keyword_end); // TODO check_match
        self.token = try self.lexer.next();

        var if_statement = try self.arena.create(Node.IfStatement);
        if_statement.* = .{
            .clauses = try self.arena.dupe(*Node, clauses.items),
        };
        return &if_statement.base;
    }

    fn localfunc(self: *Self) Error!*Node {
        unreachable;
    }

    fn localstat(self: *Self) Error!*Node {
        var names = std.ArrayList(Token).init(self.allocator);
        defer names.deinit();

        while (true) {
            std.debug.assert(self.token.id == .name); // TODO check()
            try names.append(self.token);
            self.token = try self.lexer.next();

            if (!try self.testcharnext(',')) break;
        }

        var values = std.ArrayList(*Node).init(self.allocator);
        defer values.deinit();

        if (try self.testcharnext('=')) {
            _ = try self.explist1(&values);
        }

        var local = try self.arena.create(Node.LocalStatement);
        local.* = .{
            .names = try self.arena.dupe(Token, names.items),
            .values = try self.arena.dupe(*Node, values.items),
        };
        return &local.base;
    }

    fn exprstat(self: *Self) Error!*Node {
        return self.primaryexp();
    }

    fn explist1(self: *Self, list: *std.ArrayList(*Node)) Error!usize {
        var num_expressions: usize = 1;
        try list.append(try self.expr());
        while (try self.testcharnext(',')) {
            try list.append(try self.expr());
        }
        return num_expressions;
    }

    /// simpleexp -> NUMBER | STRING | NIL | true | false | ... |
    ///              constructor | FUNCTION body | primaryexp
    fn simpleexp(self: *Self) Error!*Node {
        switch (self.token.id) {
            .string, .number, .keyword_false, .keyword_true, .keyword_nil, .ellipsis => {
                // TODO: "cannot use ... outside a vararg function"
                // or should that be a compile error instead?
                const node = try self.arena.create(Node.Literal);
                node.* = .{
                    .token = self.token,
                };
                self.token = try self.lexer.next();
                return &node.base;
            },
            .single_char => {
                switch (self.token.char.?) {
                    '{' => unreachable, // TODO constructor
                    else => {},
                }
            },
            .keyword_function => unreachable, // TODO
            else => {},
        }
        return self.primaryexp();
    }

    fn expr(self: *Self) Error!*Node {
        return self.simpleexp();
    }

    fn primaryexp(self: *Self) Error!*Node {
        var arguments = std.ArrayList(*Node).init(self.allocator);
        defer arguments.deinit();

        var node = try self.prefixexp();
        var is_func_call = false;
        loop: while (true) {
            switch (self.token.id) {
                .single_char => {
                    switch (self.token.char.?) {
                        '.' => {
                            const separator = self.token;
                            self.token = try self.lexer.next(); // skip the dot
                            std.debug.assert(self.token.id == .name); // TODO check()

                            var new_node = try self.arena.create(Node.FieldAccess);
                            new_node.* = .{
                                .prefix = node,
                                .field = self.token,
                                .separator = separator,
                            };
                            node = &new_node.base;

                            self.token = try self.lexer.next();
                        },
                        '[' => {
                            self.token = try self.lexer.next(); // skip the [
                            const index = try self.expr();
                            std.debug.assert(self.token.id == .single_char and self.token.char.? == ']'); // TODO check()

                            var new_node = try self.arena.create(Node.IndexAccess);
                            new_node.* = .{
                                .prefix = node,
                                .index = index,
                            };
                            node = &new_node.base;

                            self.token = try self.lexer.next();
                        },
                        ':' => {
                            const separator = self.token;
                            self.token = try self.lexer.next(); // skip the :
                            std.debug.assert(self.token.id == .name); // TODO check()

                            var new_node = try self.arena.create(Node.FieldAccess);
                            new_node.* = .{
                                .prefix = node,
                                .field = self.token,
                                .separator = separator,
                            };
                            node = &new_node.base;

                            self.token = try self.lexer.next();
                            node = try self.funcargs(node);
                        },
                        '(' => {
                            is_func_call = true;
                            node = try self.funcargs(node);
                        },
                        else => break :loop,
                    }
                },
                .string => {
                    is_func_call = true;
                    node = try self.funcargs(node);
                },
                else => break :loop,
            }
        }

        return node;
    }

    fn funcargs(self: *Self, expression: *Node) Error!*Node {
        var arguments = std.ArrayList(*Node).init(self.allocator);
        defer arguments.deinit();

        switch (self.token.id) {
            .single_char => switch (self.token.char.?) {
                '(' => {
                    self.token = try self.lexer.next();
                    const has_no_arguments = self.token.id == .single_char and self.token.char.? == ')';
                    if (!has_no_arguments) {
                        _ = try self.explist1(&arguments);
                    }
                    if (!(try self.testcharnext(')'))) {
                        return error.ExpectedCloseParen;
                    }
                },
                '{' => unreachable, // TODO table constructor call
                else => {
                    return error.FunctionArgumentsExpected;
                },
            },
            .string => {
                const node = try self.arena.create(Node.Literal);
                node.* = .{
                    .token = self.token,
                };
                try arguments.append(&node.base);
                self.token = try self.lexer.next();
            },
            else => return error.FunctionArgumentsExpected,
        }

        var call = try self.arena.create(Node.Call);
        call.* = .{
            .expression = expression,
            .arguments = try self.arena.dupe(*Node, arguments.items),
        };
        return &call.base;
    }

    fn prefixexp(self: *Self) Error!*Node {
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

test "check hello world ast" {
    const allocator = std.testing.allocator;
    var tree = try parse(allocator, "print \"hello world\"");
    defer tree.deinit();

    std.testing.expectEqual(Node.Id.chunk, tree.node.id);
    const chunk = tree.node.cast(.chunk).?;
    std.testing.expectEqual(@as(usize, 1), chunk.body.len);
    std.testing.expectEqual(Node.Id.call, chunk.body[0].id);
    const call = chunk.body[0].cast(.call).?;
    std.testing.expectEqual(Node.Id.identifier, call.expression.id);
    std.testing.expectEqual(@as(usize, 1), call.arguments.len);
    std.testing.expectEqual(Node.Id.literal, call.arguments[0].id);
}

fn testParse(source: []const u8, expected_ast_dump: []const u8) !void {
    const allocator = std.testing.allocator;
    var tree = try parse(allocator, source);
    defer tree.deinit();

    var buf = std.ArrayList(u8).init(allocator);
    defer buf.deinit();

    try tree.dump(buf.writer());
    std.testing.expectEqualStrings(expected_ast_dump, buf.items);
}

test "hello world" {
    try testParse("print \"hello world\"",
        \\chunk
        \\ call
        \\  identifier
        \\  (
        \\   literal <string>
        \\  )
        \\
    );
    try testParse("print(\"hello world\")",
        \\chunk
        \\ call
        \\  identifier
        \\  (
        \\   literal <string>
        \\  )
        \\
    );
}

test "function call" {
    try testParse("print(123)",
        \\chunk
        \\ call
        \\  identifier
        \\  (
        \\   literal <number>
        \\  )
        \\
    );
    try testParse("print(\"hello\", 'world', nil, true, false, 123)",
        \\chunk
        \\ call
        \\  identifier
        \\  (
        \\   literal <string>
        \\   literal <string>
        \\   literal nil
        \\   literal true
        \\   literal false
        \\   literal <number>
        \\  )
        \\
    );
    // chained
    try testParse("a()()()",
        \\chunk
        \\ call
        \\  call
        \\   call
        \\    identifier
        \\    ()
        \\   ()
        \\  ()
        \\
    );
}

test "local statements" {
    try testParse("local x, y",
        \\chunk
        \\ local_statement <name> <name>
        \\
    );
    try testParse("local x = nil",
        \\chunk
        \\ local_statement <name> =
        \\  literal nil
        \\
    );
    try testParse("local x, y = nil, true",
        \\chunk
        \\ local_statement <name> <name> =
        \\  literal nil
        \\  literal true
        \\
    );
    try testParse("local x, y = z",
        \\chunk
        \\ local_statement <name> <name> =
        \\  identifier
        \\
    );
}

test "field and index access" {
    try testParse("z.a",
        \\chunk
        \\ field_access .<name>
        \\  identifier
        \\
    );
    try testParse("z.a.b.c",
        \\chunk
        \\ field_access .<name>
        \\  field_access .<name>
        \\   field_access .<name>
        \\    identifier
        \\
    );
    try testParse("z.a['b'].c",
        \\chunk
        \\ field_access .<name>
        \\  index_access
        \\   field_access .<name>
        \\    identifier
        \\   literal <string>
        \\
    );
    try testParse("z.a[b.a[1]].c",
        \\chunk
        \\ field_access .<name>
        \\  index_access
        \\   field_access .<name>
        \\    identifier
        \\   index_access
        \\    field_access .<name>
        \\     identifier
        \\    literal <number>
        \\
    );
    try testParse("a.b:c 'd'",
        \\chunk
        \\ call
        \\  field_access :<name>
        \\   field_access .<name>
        \\    identifier
        \\  (
        \\   literal <string>
        \\  )
        \\
    );
}

test "if statements" {
    try testParse("if a then b() end",
        \\chunk
        \\ if_statement
        \\  if_clause if
        \\   identifier
        \\  then
        \\   call
        \\    identifier
        \\    ()
        \\
    );
    try testParse("if a then elseif b then elseif true then else end",
        \\chunk
        \\ if_statement
        \\  if_clause if
        \\   identifier
        \\  then
        \\  if_clause elseif
        \\   identifier
        \\  then
        \\  if_clause elseif
        \\   literal true
        \\  then
        \\  if_clause else
        \\
    );
}
