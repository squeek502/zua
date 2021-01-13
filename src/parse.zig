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
    ExpectedEqualsOrIn,
    ExpectedNameOrVarArg,
    UnexpectedSymbol,
    SyntaxError,
    ExpectedDifferentToken, // error_expected in lparser.c
};

pub const Parser = struct {
    const Self = @This();

    lexer: *Lexer,
    token: Token,
    allocator: *Allocator,
    arena: *Allocator,

    pub const Error = ParseError || Lexer.Error || Allocator.Error;

    pub const PossibleLValueExpression = struct {
        node: *Node,
        can_be_assigned_to: bool = true,
    };

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
            .keyword_return => return self.retstat(),
            .keyword_while => return self.whilestat(),
            .keyword_do => return self.dostat(),
            .keyword_repeat => return self.repeatstat(),
            .keyword_break => return self.breakstat(),
            .keyword_for => return self.forstat(),
            .keyword_function => return self.funcstat(),
            else => return self.exprstat(),
        }
    }

    fn block(self: *Self, list: *std.ArrayList(*Node)) Error!void {
        while (!block_follow(self.token)) {
            try list.append(try self.statement());
            _ = try self.testcharnext(';');
        }
    }

    /// parlist -> [ param { `,' param } ]
    fn parlist(self: *Self, list: *std.ArrayList(Token)) Error!void {
        // no params
        if (self.token.isChar(')')) return;

        var found_vararg = false;
        while (true) {
            switch (self.token.id) {
                .name => {},
                .ellipsis => {
                    found_vararg = true;
                },
                else => return error.ExpectedNameOrVarArg,
            }
            try list.append(self.token);
            self.token = try self.lexer.next();
            if (found_vararg or !try self.testcharnext(',')) {
                break;
            }
        }
    }

    /// body ->  `(' parlist `)' chunk END
    fn funcbody(self: *Self, name: ?*Node, is_local: bool) Error!*Node {
        try self.checkcharnext('(');

        var params = std.ArrayList(Token).init(self.allocator);
        defer params.deinit();

        try self.parlist(&params);

        try self.checkcharnext(')');

        var body = std.ArrayList(*Node).init(self.allocator);
        defer body.deinit();

        try self.block(&body);

        try self.checknext(.keyword_end); // TODO check_match

        const node = try self.arena.create(Node.FunctionDeclaration);
        node.* = .{
            .name = name,
            .parameters = try self.arena.dupe(Token, params.items),
            .body = try self.arena.dupe(*Node, body.items),
            .is_local = is_local,
        };
        return &node.base;
    }

    // funcstat -> FUNCTION funcname body
    fn funcstat(self: *Self) Error!*Node {
        std.debug.assert(self.token.id == .keyword_function);
        self.token = try self.lexer.next();

        const name = try self.funcname();
        return self.funcbody(name, false);
    }

    // funcname -> NAME {field} [`:' NAME]
    fn funcname(self: *Self) Error!*Node {
        const name_token = try self.checkname();
        var identifier_node = try self.arena.create(Node.Identifier);
        identifier_node.* = .{
            .token = name_token,
        };
        var node = &identifier_node.base;

        while (self.token.isChar('.')) {
            const separator = self.token;
            self.token = try self.lexer.next(); // skip separator
            const field_token = try self.checkname();

            var new_node = try self.arena.create(Node.FieldAccess);
            new_node.* = .{
                .prefix = node,
                .field = field_token,
                .separator = separator,
            };
            node = &new_node.base;
        }
        if (self.token.isChar(':')) {
            const separator = self.token;
            self.token = try self.lexer.next();
            const field_token = try self.checkname();

            var new_node = try self.arena.create(Node.FieldAccess);
            new_node.* = .{
                .prefix = node,
                .field = field_token,
                .separator = separator,
            };
            node = &new_node.base;
        }

        return node;
    }

    /// stat -> RETURN explist
    fn retstat(self: *Self) Error!*Node {
        std.debug.assert(self.token.id == .keyword_return);
        self.token = try self.lexer.next();

        var return_values = std.ArrayList(*Node).init(self.allocator);
        defer return_values.deinit();

        const no_return_values = block_follow(self.token) or (self.token.isChar(';'));
        if (!no_return_values) {
            _ = try self.explist1(&return_values);
        }

        const node = try self.arena.create(Node.ReturnStatement);
        node.* = .{
            .values = try self.arena.dupe(*Node, return_values.items),
        };
        return &node.base;
    }

    /// cond -> exp
    fn cond(self: *Self) Error!*Node {
        return self.expr();
    }

    /// whilestat -> WHILE cond DO block END
    fn whilestat(self: *Self) Error!*Node {
        std.debug.assert(self.token.id == .keyword_while);
        self.token = try self.lexer.next();

        const condition = try self.cond();

        try self.checknext(.keyword_do);

        var body = std.ArrayList(*Node).init(self.allocator);
        defer body.deinit();

        try self.block(&body);

        try self.checknext(.keyword_end); // TODO check_match

        var while_statement = try self.arena.create(Node.WhileStatement);
        while_statement.* = .{
            .condition = condition,
            .body = try self.arena.dupe(*Node, body.items),
        };
        return &while_statement.base;
    }

    fn breakstat(self: *Self) Error!*Node {
        std.debug.assert(self.token.id == .keyword_break);
        const break_token = self.token;
        self.token = try self.lexer.next();

        var break_statement = try self.arena.create(Node.BreakStatement);
        break_statement.* = .{
            .token = break_token,
        };
        return &break_statement.base;
    }

    fn dostat(self: *Self) Error!*Node {
        std.debug.assert(self.token.id == .keyword_do);
        self.token = try self.lexer.next();

        var body = std.ArrayList(*Node).init(self.allocator);
        defer body.deinit();

        try self.block(&body);

        try self.checknext(.keyword_end); // TODO: check_match

        const node = try self.arena.create(Node.DoStatement);
        node.* = .{
            .body = try self.arena.dupe(*Node, body.items),
        };
        return &node.base;
    }

    /// repeatstat -> REPEAT block UNTIL cond
    fn repeatstat(self: *Self) Error!*Node {
        std.debug.assert(self.token.id == .keyword_repeat);
        self.token = try self.lexer.next();

        var body = std.ArrayList(*Node).init(self.allocator);
        defer body.deinit();

        try self.block(&body);

        try self.checknext(.keyword_until); // TODO: check_match

        const condition = try self.cond();

        const node = try self.arena.create(Node.RepeatStatement);
        node.* = .{
            .condition = condition,
            .body = try self.arena.dupe(*Node, body.items),
        };
        return &node.base;
    }

    /// forstat -> FOR (fornum | forlist) END
    fn forstat(self: *Self) Error!*Node {
        std.debug.assert(self.token.id == .keyword_for);
        self.token = try self.lexer.next();

        const name_token = try self.checkname();

        var for_node: *Node = undefined;

        switch (self.token.id) {
            .single_char => switch (self.token.char.?) {
                '=' => for_node = try self.fornum(name_token),
                ',' => for_node = try self.forlist(name_token),
                else => return error.ExpectedEqualsOrIn,
            },
            .keyword_in => for_node = try self.forlist(name_token),
            else => return error.ExpectedEqualsOrIn,
        }

        try self.checknext(.keyword_end); // TODO check_match

        return for_node;
    }

    /// fornum -> NAME = exp1,exp1[,exp1] forbody
    fn fornum(self: *Self, name_token: Token) Error!*Node {
        try self.checkcharnext('=');

        const start_expression = try self.exp1();

        try self.checkcharnext(',');

        const end_expression = try self.exp1();

        var increment_expression: ?*Node = null;
        if (try self.testcharnext(',')) {
            increment_expression = try self.exp1();
        }

        try self.checknext(.keyword_do);

        var body = std.ArrayList(*Node).init(self.allocator);
        defer body.deinit();

        try self.block(&body);

        var for_node = try self.arena.create(Node.ForStatementNumeric);
        for_node.* = .{
            .name = name_token,
            .start = start_expression,
            .end = end_expression,
            .increment = increment_expression,
            .body = try self.arena.dupe(*Node, body.items),
        };
        return &for_node.base;
    }

    /// forlist -> NAME {,NAME} IN explist1 forbody
    fn forlist(self: *Self, first_name_token: Token) Error!*Node {
        var names = try std.ArrayList(Token).initCapacity(self.allocator, 1);
        defer names.deinit();

        names.appendAssumeCapacity(first_name_token);

        while (try self.testcharnext(',')) {
            const name_token = try self.checkname();
            try names.append(name_token);
        }
        try self.checknext(.keyword_in);

        var expressions = std.ArrayList(*Node).init(self.allocator);
        defer expressions.deinit();

        _ = try self.explist1(&expressions);

        try self.checknext(.keyword_do);

        var body = std.ArrayList(*Node).init(self.allocator);
        defer body.deinit();

        try self.block(&body);

        var for_node = try self.arena.create(Node.ForStatementGeneric);
        for_node.* = .{
            .names = try self.arena.dupe(Token, names.items),
            .expressions = try self.arena.dupe(*Node, expressions.items),
            .body = try self.arena.dupe(*Node, body.items),
        };
        return &for_node.base;
    }

    /// sort of the equivalent of Lua's test_then_block in lparser.c but handles else too
    fn ifclause(self: *Self) Error!*Node {
        std.debug.assert(self.token.id == .keyword_if or self.token.id == .keyword_elseif or self.token.id == .keyword_else);

        const if_token = self.token;
        var condition: ?*Node = null;

        self.token = try self.lexer.next();
        switch (if_token.id) {
            .keyword_if, .keyword_elseif => {
                condition = try self.cond();
                try self.checknext(.keyword_then);
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

        try self.checknext(.keyword_end); // TODO check_match

        var if_statement = try self.arena.create(Node.IfStatement);
        if_statement.* = .{
            .clauses = try self.arena.dupe(*Node, clauses.items),
        };
        return &if_statement.base;
    }

    fn localfunc(self: *Self) Error!*Node {
        const name_token = try self.checkname();
        var name = try self.arena.create(Node.Identifier);
        name.* = .{ .token = name_token };
        return self.funcbody(&name.base, true);
    }

    /// listfield -> expr
    fn listfield(self: *Self) Error!*Node {
        var value = try self.expr();
        var node = try self.arena.create(Node.TableField);
        node.* = .{
            .key = null,
            .value = value,
        };
        return &node.base;
    }

    /// recfield -> (NAME | `['exp1`]') = exp1
    fn recfield(self: *Self) Error!*Node {
        var key: *Node = get_key: {
            if (self.token.id == .name) { // TODO checkname
                const name_token = try self.checkname();
                // This might be kinda weird, but the name token here is actually used as
                // more of a string literal, so create a Literal node instead of Identifier.
                // This is a special case.
                // TODO revisit this?
                var name_node = try self.arena.create(Node.Literal);
                name_node.* = .{ .token = name_token };
                break :get_key &name_node.base;
            } else {
                std.debug.assert(self.token.isChar('['));
                self.token = try self.lexer.next(); // skip the [

                const key_expr = try self.expr();

                try self.checkcharnext(']');

                break :get_key key_expr;
            }
        };
        try self.checkcharnext('=');

        const value = try self.expr();

        var node = try self.arena.create(Node.TableField);
        node.* = .{
            .key = key,
            .value = value,
        };
        return &node.base;
    }

    /// constructor -> ??
    fn constructor(self: *Self) Error!*Node {
        try self.checkcharnext('{');

        var fields = std.ArrayList(*Node).init(self.allocator);
        defer fields.deinit();

        while (!self.token.isChar('}')) {
            var field: *Node = get_field: {
                switch (self.token.id) {
                    .name => {
                        // TODO presumably should catch EOF here and translate it to an appropriate error
                        const lookahead_token = try self.lexer.lookahead();
                        if (lookahead_token.isChar('=')) {
                            break :get_field try self.recfield();
                        } else {
                            break :get_field try self.listfield();
                        }
                    },
                    .single_char => switch (self.token.char.?) {
                        '[' => break :get_field try self.recfield(),
                        else => break :get_field try self.listfield(),
                    },
                    else => break :get_field try self.listfield(),
                }
            };
            try fields.append(field);

            const has_more = (try self.testcharnext(',')) or (try self.testcharnext(';'));
            if (!has_more) break;
        }

        try self.checkcharnext('}'); // TODO check_match

        var node = try self.arena.create(Node.TableConstructor);
        node.* = .{
            .fields = try self.arena.dupe(*Node, fields.items),
        };
        return &node.base;
    }

    /// stat -> LOCAL NAME {`,' NAME} [`=' explist1]
    fn localstat(self: *Self) Error!*Node {
        return try self.assignment(null);
    }

    /// if first_variable is null, then this is a local assignment
    fn assignment(self: *Self, first_variable: ?PossibleLValueExpression) Error!*Node {
        const is_local = first_variable == null;

        var variables = std.ArrayList(*Node).init(self.allocator);
        defer variables.deinit();

        var values = std.ArrayList(*Node).init(self.allocator);
        defer values.deinit();

        if (is_local) {
            while (true) {
                const name_token = try self.checkname();
                var identifier = try self.arena.create(Node.Identifier);
                identifier.* = .{ .token = name_token };
                try variables.append(&identifier.base);

                if (!try self.testcharnext(',')) break;
            }
            if (try self.testcharnext('=')) {
                _ = try self.explist1(&values);
            }
        } else {
            var variable = first_variable.?;
            while (true) {
                if (!variable.can_be_assigned_to) {
                    return error.SyntaxError;
                }
                try variables.append(variable.node);
                if (try self.testcharnext(',')) {
                    variable = try self.primaryexp();
                } else {
                    break;
                }
            }
            try self.checkcharnext('=');
            _ = try self.explist1(&values);
        }

        var local = try self.arena.create(Node.AssignmentStatement);
        local.* = .{
            .variables = try self.arena.dupe(*Node, variables.items),
            .values = try self.arena.dupe(*Node, values.items),
            .is_local = is_local,
        };
        return &local.base;
    }

    /// stat -> func | assignment
    fn exprstat(self: *Self) Error!*Node {
        var expression = try self.primaryexp();
        // if it's not a call, then it's an assignment
        if (expression.node.id != .call) {
            expression.node = try self.assignment(expression);
        }
        return expression.node;
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
                    '{' => return self.constructor(),
                    else => {},
                }
            },
            .keyword_function => {
                self.token = try self.lexer.next(); // skip function
                return self.funcbody(null, false);
            },
            else => {},
        }
        const expression = try self.primaryexp();
        return expression.node;
    }

    fn expr(self: *Self) Error!*Node {
        return self.simpleexp();
    }

    // TODO could probably be eliminated in favor of just calling expr directly
    fn exp1(self: *Self) Error!*Node {
        return self.expr();
    }

    fn primaryexp(self: *Self) Error!PossibleLValueExpression {
        var arguments = std.ArrayList(*Node).init(self.allocator);
        defer arguments.deinit();

        var expression = try self.prefixexp();
        var is_func_call = false;
        loop: while (true) {
            switch (self.token.id) {
                .single_char => {
                    switch (self.token.char.?) {
                        '.' => {
                            const separator = self.token;
                            self.token = try self.lexer.next(); // skip the dot
                            const field_token = try self.checkname();

                            var new_node = try self.arena.create(Node.FieldAccess);
                            new_node.* = .{
                                .prefix = expression.node,
                                .field = field_token,
                                .separator = separator,
                            };
                            expression.node = &new_node.base;
                            expression.can_be_assigned_to = true;
                        },
                        '[' => {
                            self.token = try self.lexer.next(); // skip the [
                            const index = try self.expr();
                            try self.checkcharnext(']');

                            var new_node = try self.arena.create(Node.IndexAccess);
                            new_node.* = .{
                                .prefix = expression.node,
                                .index = index,
                            };
                            expression.node = &new_node.base;
                            expression.can_be_assigned_to = true;
                        },
                        ':' => {
                            const separator = self.token;
                            self.token = try self.lexer.next(); // skip the :
                            const field_token = try self.checkname();

                            var new_node = try self.arena.create(Node.FieldAccess);
                            new_node.* = .{
                                .prefix = expression.node,
                                .field = field_token,
                                .separator = separator,
                            };
                            expression.node = &new_node.base;
                            expression.can_be_assigned_to = false;

                            expression.node = try self.funcargs(expression.node);
                        },
                        '(', '{' => {
                            expression.node = try self.funcargs(expression.node);
                            expression.can_be_assigned_to = false;
                        },
                        else => break :loop,
                    }
                },
                .string => {
                    expression.node = try self.funcargs(expression.node);
                    expression.can_be_assigned_to = false;
                },
                else => break :loop,
            }
        }

        return expression;
    }

    fn funcargs(self: *Self, expression: *Node) Error!*Node {
        var arguments = std.ArrayList(*Node).init(self.allocator);
        defer arguments.deinit();

        switch (self.token.id) {
            .single_char => switch (self.token.char.?) {
                '(' => {
                    self.token = try self.lexer.next();
                    const has_no_arguments = self.token.isChar(')');
                    if (!has_no_arguments) {
                        _ = try self.explist1(&arguments);
                    }
                    if (!(try self.testcharnext(')'))) { // TODO check_match
                        return error.ExpectedCloseParen;
                    }
                },
                '{' => {
                    const node = try self.constructor();
                    try arguments.append(node);
                },
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

    fn prefixexp(self: *Self) Error!PossibleLValueExpression {
        switch (self.token.id) {
            .name => {
                const node = try self.arena.create(Node.Identifier);
                node.* = .{
                    .token = self.token,
                };
                self.token = try self.lexer.next();
                return PossibleLValueExpression{ .node = &node.base };
            },
            .single_char => switch (self.token.char.?) {
                '(' => {
                    self.token = try self.lexer.next();
                    const node = try self.expr();

                    try self.checkcharnext(')'); // TODO check_match

                    return PossibleLValueExpression{
                        .node = node,
                        .can_be_assigned_to = false,
                    };
                },
                else => {},
            },
            else => {},
        }
        return error.UnexpectedSymbol;
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
        if (self.token.isChar(char)) {
            self.token = try self.lexer.next();
            return true;
        }
        return false;
    }

    fn check(self: *Self, expected_id: Token.Id) !void {
        if (self.token.id != expected_id) return error.ExpectedDifferentToken;
    }

    fn checkchar(self: *Self, expected_char: u8) !void {
        if (!self.token.isChar(expected_char)) return error.ExpectedDifferentToken;
    }

    fn checknext(self: *Self, expected_id: Token.Id) !void {
        try self.check(expected_id);
        self.token = try self.lexer.next();
    }

    fn checkcharnext(self: *Self, expected_char: u8) !void {
        try self.checkchar(expected_char);
        self.token = try self.lexer.next();
    }

    // TODO eliminate?
    /// Returns the name token, since it is typically used when constructing the AST node
    fn checkname(self: *Self) !Token {
        const token = self.token;
        try self.checknext(.name);
        return token;
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

fn expectParseError(expected: ParseError, source: []const u8) void {
    std.testing.expectError(expected, testParse(source, ""));
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
    // table constructor
    try testParse("a{}",
        \\chunk
        \\ call
        \\  identifier
        \\  (
        \\   table_constructor
        \\  )
        \\
    );
}

test "local statements" {
    try testParse("local x, y",
        \\chunk
        \\ assignment_statement local
        \\  identifier
        \\  identifier
        \\
    );
    try testParse("local x = nil",
        \\chunk
        \\ assignment_statement local
        \\  identifier
        \\ =
        \\  literal nil
        \\
    );
    try testParse("local x, y = nil, true",
        \\chunk
        \\ assignment_statement local
        \\  identifier
        \\  identifier
        \\ =
        \\  literal nil
        \\  literal true
        \\
    );
    try testParse("local x, y = z",
        \\chunk
        \\ assignment_statement local
        \\  identifier
        \\  identifier
        \\ =
        \\  identifier
        \\
    );
}

test "field and index access" {
    try testParse("z.a = nil",
        \\chunk
        \\ assignment_statement
        \\  field_access .<name>
        \\   identifier
        \\ =
        \\  literal nil
        \\
    );
    try testParse("z.a.b.c = nil",
        \\chunk
        \\ assignment_statement
        \\  field_access .<name>
        \\   field_access .<name>
        \\    field_access .<name>
        \\     identifier
        \\ =
        \\  literal nil
        \\
    );
    try testParse("z.a['b'].c = nil",
        \\chunk
        \\ assignment_statement
        \\  field_access .<name>
        \\   index_access
        \\    field_access .<name>
        \\     identifier
        \\    literal <string>
        \\ =
        \\  literal nil
        \\
    );
    try testParse("z.a[b.a[1]].c = nil",
        \\chunk
        \\ assignment_statement
        \\  field_access .<name>
        \\   index_access
        \\    field_access .<name>
        \\     identifier
        \\    index_access
        \\     field_access .<name>
        \\      identifier
        \\     literal <number>
        \\ =
        \\  literal nil
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

test "return statements" {
    try testParse("return",
        \\chunk
        \\ return_statement
        \\
    );
    try testParse("return nil, true, x, 'a', b()",
        \\chunk
        \\ return_statement
        \\  literal nil
        \\  literal true
        \\  identifier
        \\  literal <string>
        \\  call
        \\   identifier
        \\   ()
        \\
    );
}

test "while statements" {
    try testParse("while a do b() end",
        \\chunk
        \\ while_statement
        \\  identifier
        \\ do
        \\  call
        \\   identifier
        \\   ()
        \\
    );
}

test "do statements" {
    try testParse("do b() end",
        \\chunk
        \\ do_statement
        \\  call
        \\   identifier
        \\   ()
        \\
    );
}

test "repeat statements" {
    try testParse("repeat b() until a",
        \\chunk
        \\ repeat_statement
        \\  call
        \\   identifier
        \\   ()
        \\ until
        \\  identifier
        \\
    );
}

test "break statements" {
    try testParse("break",
        \\chunk
        \\ break_statement
        \\
    );
}

test "numeric for statements" {
    try testParse("for i=1,2 do end",
        \\chunk
        \\ for_statement_numeric
        \\  literal <number>
        \\  literal <number>
        \\ do
        \\
    );
    try testParse("for i=1,2,a do b() end",
        \\chunk
        \\ for_statement_numeric
        \\  literal <number>
        \\  literal <number>
        \\  identifier
        \\ do
        \\  call
        \\   identifier
        \\   ()
        \\
    );
}

test "generic for statements" {
    try testParse("for k,v in ipairs(a) do end",
        \\chunk
        \\ for_statement_generic <name> <name>
        \\ in
        \\  call
        \\   identifier
        \\   (
        \\    identifier
        \\   )
        \\ do
        \\
    );
    try testParse("for a in b,c,d,e do f() end",
        \\chunk
        \\ for_statement_generic <name>
        \\ in
        \\  identifier
        \\  identifier
        \\  identifier
        \\  identifier
        \\ do
        \\  call
        \\   identifier
        \\   ()
        \\
    );
}

test "function declarations" {
    try testParse("function a() end",
        \\chunk
        \\ function_declaration
        \\  identifier
        \\  ()
        \\
    );
    try testParse("local function a() end",
        \\chunk
        \\ function_declaration local
        \\  identifier
        \\  ()
        \\
    );
    try testParse("function a.b:c() end",
        \\chunk
        \\ function_declaration
        \\  field_access :<name>
        \\   field_access .<name>
        \\    identifier
        \\  ()
        \\
    );
    try testParse("local function a(b, c, ...) end",
        \\chunk
        \\ function_declaration local
        \\  identifier
        \\  (<name> <name> ...)
        \\
    );
    try testParse("function a(b, c, ...) end",
        \\chunk
        \\ function_declaration
        \\  identifier
        \\  (<name> <name> ...)
        \\
    );
}

test "anonymous function" {
    try testParse("local a = function() end",
        \\chunk
        \\ assignment_statement local
        \\  identifier
        \\ =
        \\  function_declaration
        \\   ()
        \\
    );
}

test "assignment" {
    try testParse("test = nil",
        \\chunk
        \\ assignment_statement
        \\  identifier
        \\ =
        \\  literal nil
        \\
    );
    try testParse("a, b, c.d, e[f] = nil, true",
        \\chunk
        \\ assignment_statement
        \\  identifier
        \\  identifier
        \\  field_access .<name>
        \\   identifier
        \\  index_access
        \\   identifier
        \\   identifier
        \\ =
        \\  literal nil
        \\  literal true
        \\
    );
    try testParse("(a).b = nil",
        \\chunk
        \\ assignment_statement
        \\  field_access .<name>
        \\   identifier
        \\ =
        \\  literal nil
        \\
    );
    try testParse("a().b = nil",
        \\chunk
        \\ assignment_statement
        \\  field_access .<name>
        \\   call
        \\    identifier
        \\    ()
        \\ =
        \\  literal nil
        \\
    );
}

test "assignment errors" {
    expectParseError(error.SyntaxError, "(a) = nil");
    expectParseError(error.UnexpectedSymbol, "(a)() = nil");
    expectParseError(error.FunctionArgumentsExpected, "a:b = nil");
    expectParseError(error.SyntaxError, "(a)");
    expectParseError(error.UnexpectedSymbol, "true = nil");
}

test "table constructors" {
    try testParse("a = {}",
        \\chunk
        \\ assignment_statement
        \\  identifier
        \\ =
        \\  table_constructor
        \\
    );
    try testParse("a = {1;nil,something}",
        \\chunk
        \\ assignment_statement
        \\  identifier
        \\ =
        \\  table_constructor
        \\   table_field
        \\    literal <number>
        \\   table_field
        \\    literal nil
        \\   table_field
        \\    identifier
        \\
    );
    try testParse("a = {b=true}",
        \\chunk
        \\ assignment_statement
        \\  identifier
        \\ =
        \\  table_constructor
        \\   table_field
        \\    literal <name>
        \\   =
        \\    literal true
        \\
    );
    try testParse("a = {[true]=1}",
        \\chunk
        \\ assignment_statement
        \\  identifier
        \\ =
        \\  table_constructor
        \\   table_field
        \\    literal true
        \\   =
        \\    literal <number>
        \\
    );
    try testParse("a = {[something]=1}",
        \\chunk
        \\ assignment_statement
        \\  identifier
        \\ =
        \\  table_constructor
        \\   table_field
        \\    identifier
        \\   =
        \\    literal <number>
        \\
    );
    try testParse("a = {[\"something\"]=1}",
        \\chunk
        \\ assignment_statement
        \\  identifier
        \\ =
        \\  table_constructor
        \\   table_field
        \\    literal <string>
        \\   =
        \\    literal <number>
        \\
    );
    try testParse("a = {[{}]={},{}}",
        \\chunk
        \\ assignment_statement
        \\  identifier
        \\ =
        \\  table_constructor
        \\   table_field
        \\    table_constructor
        \\   =
        \\    table_constructor
        \\   table_field
        \\    table_constructor
        \\
    );
}
