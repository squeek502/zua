const std = @import("std");
const Allocator = std.mem.Allocator;
const zua = @import("zua.zig");
const Lexer = @import("lex.zig").Lexer;
const LexErrorContext = zua.lex.LexErrorContext;
const Token = @import("lex.zig").Token;
const Node = @import("ast.zig").Node;
const Tree = @import("ast.zig").Tree;
const AutoComptimeLookup = @import("comptime_lookup.zig").AutoComptimeLookup;

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

/// SHRT_MAX, only used when checking the local var vector size
pub const max_local_vars = std.math.maxInt(i16);

/// LUAI_MAXVARS in luaconf.h
pub const max_local_vars_per_func = 200;

/// LUAI_MAXCCALLS in luaconf.h
pub const max_syntax_levels = 200;

/// From llimits.h
pub const max_stack_size = 250;

pub const ParseError = error{
    FunctionArgumentsExpected,
    ExpectedEqualsOrIn,
    ExpectedNameOrVarArg,
    UnexpectedSymbol,
    SyntaxError,
    ExpectedDifferentToken, // error_expected in lparser.c
    TooManyLocalVariables,
    NoLoopToBreak,
    TooManySyntaxLevels,
    AmbiguousSyntax,
    VarArgOutsideVarArgFunction,
};

/// error -> msg lookup for parse errors
pub const parse_error_strings = AutoComptimeLookup(ParseError, []const u8, .{
    .{ ParseError.FunctionArgumentsExpected, "function arguments expected" },
    .{ ParseError.ExpectedEqualsOrIn, "'=' or 'in' expected" },
    .{ ParseError.ExpectedNameOrVarArg, "<name> or '...' expected" },
    .{ ParseError.UnexpectedSymbol, "unexpected symbol" },
    .{ ParseError.SyntaxError, "syntax error" },
    //.{ ParseError.ExpectedDifferentToken, ... }, this is context-specific
    .{ ParseError.TooManyLocalVariables, "too many local variables" },
    .{ ParseError.NoLoopToBreak, "no loop to break" },
    .{ ParseError.TooManySyntaxLevels, "chunk has too many syntax levels" },
    .{ ParseError.AmbiguousSyntax, "ambiguous syntax (function call x new statement)" },
    .{ ParseError.VarArgOutsideVarArgFunction, "cannot use '...' outside a vararg function" },
});

// TODO this is duplcated from lex.zig, should be combined in the future
pub const ParseErrorContext = struct {
    token: Token,
    expected: ?Token = null,
    expected_match: ?Token = null,
    // TODO this is kinda weird, doesn't seem like it needs to be stored (maybe passed to render instead?)
    err: ParseError,

    pub fn renderAlloc(self: *ParseErrorContext, allocator: *Allocator, parser: *Parser) ![]const u8 {
        var buffer = std.ArrayList(u8).init(allocator);
        errdefer buffer.deinit();

        var msg_buf: [256]u8 = undefined;
        const msg = msg: {
            if (self.err == ParseError.ExpectedDifferentToken) {
                var msg_fbs = std.io.fixedBufferStream(&msg_buf);
                const msg_writer = msg_fbs.writer();
                msg_writer.print("'{s}' expected", .{self.expected.?.nameForDisplay()}) catch unreachable;
                if (self.expected_match != null and self.expected_match.?.line_number != self.token.line_number) {
                    msg_writer.print(" (to close '{s}' at line {d})", .{
                        self.expected_match.?.nameForDisplay(),
                        self.expected_match.?.line_number,
                    }) catch unreachable;
                }
                break :msg msg_fbs.getWritten();
            } else {
                break :msg parse_error_strings.get(self.err).?;
            }
        };
        const error_writer = buffer.writer();
        const MAXSRC = 80; // see MAXSRC in llex.c
        var chunk_id_buf: [MAXSRC]u8 = undefined;
        const chunk_id = zua.object.getChunkId(parser.lexer.chunk_name, &chunk_id_buf);
        try error_writer.print("{s}:{d}: {s}", .{ chunk_id, self.token.line_number, msg });
        // special case for 1:1 compatibility with Lua's errors
        if (!self.token.isChar(0) and self.err != ParseError.TooManySyntaxLevels) {
            try error_writer.print(" near '{s}'", .{self.token.nameForErrorDisplay(parser.lexer.buffer)});
        }
        return buffer.toOwnedSlice();
    }
};

pub const Parser = struct {
    const Self = @This();

    lexer: *Lexer,
    error_context: ?ParseErrorContext = null,
    /// values that need to be initialized per-parse
    state: Parser.State = undefined,

    pub const Error = ParseError || Lexer.Error || Allocator.Error;

    pub fn init(lexer: *Lexer) Parser {
        return Parser{
            .lexer = lexer,
        };
    }

    pub const State = struct {
        token: Token,
        allocator: *Allocator,
        arena: *Allocator,
        in_loop: bool = false,
        in_vararg_func: bool = true, // main chunk is always vararg
        syntax_level: usize = 0,
    };

    pub fn parse(self: *Self, allocator: *Allocator) Error!*Tree {
        var arena = std.heap.ArenaAllocator.init(allocator);
        errdefer arena.deinit();

        self.state = Parser.State{
            .token = try self.lexer.next(),
            .allocator = allocator,
            .arena = &arena.allocator,
        };

        const parsed_chunk = try self.chunk();

        const tree = try self.state.arena.create(Tree);
        tree.* = .{
            .node = parsed_chunk,
            .source = self.lexer.buffer,
            .arena = arena.state,
            .allocator = allocator,
        };
        return tree;
    }

    pub const PossibleLValueExpression = struct {
        node: *Node,
        can_be_assigned_to: bool = true,
    };

    /// chunk -> { stat [`;'] }
    fn chunk(self: *Self) Error!*Node {
        var statements = std.ArrayList(*Node).init(self.state.allocator);
        defer statements.deinit();

        try self.block(&statements);
        try self.check(.eof);

        const node = try self.state.arena.create(Node.Chunk);
        node.* = .{
            .body = try self.state.arena.dupe(*Node, statements.items),
        };
        return &node.base;
    }

    fn statement(self: *Self) Error!*Node {
        switch (self.state.token.id) {
            .keyword_if => return self.ifstat(),
            .keyword_local => {
                try self.nextToken();
                const possible_function_token = self.state.token;
                if (try self.testnext(.keyword_function)) {
                    return self.localfunc(possible_function_token);
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
        try self.enterlevel();
        while (!blockFollow(self.state.token)) {
            const stat = try self.statement();
            try list.append(stat);
            _ = try self.testcharnext(';');
            const must_be_last_statement = stat.id == .return_statement or stat.id == .break_statement;
            if (must_be_last_statement) {
                break;
            }
        }
        self.leavelevel();
    }

    /// parlist -> [ param { `,' param } ]
    /// Returns true if vararg was found in the parameter list
    fn parlist(self: *Self, list: *std.ArrayList(Token)) Error!bool {
        // no params
        if (self.state.token.isChar(')')) return false;

        var found_vararg = false;
        while (true) {
            switch (self.state.token.id) {
                .name => {},
                .ellipsis => {
                    found_vararg = true;
                },
                else => return self.reportParseError(ParseError.ExpectedNameOrVarArg),
            }
            try list.append(self.state.token);
            try self.nextToken();
            if (found_vararg or !try self.testcharnext(',')) {
                break;
            }
        }
        return found_vararg;
    }

    /// body ->  `(' parlist `)' chunk END
    fn funcbody(self: *Self, function_token: Token, name: ?*Node, is_local: bool) Error!*Node {
        try self.checkcharnext('(');

        var params = std.ArrayList(Token).init(self.state.allocator);
        defer params.deinit();

        const vararg_found = try self.parlist(&params);

        try self.checkcharnext(')');

        var body = std.ArrayList(*Node).init(self.state.allocator);
        defer body.deinit();

        const in_vararg_func_prev = self.state.in_vararg_func;
        self.state.in_vararg_func = vararg_found;
        try self.block(&body);
        self.state.in_vararg_func = in_vararg_func_prev;

        try self.check_match(.keyword_end, function_token);

        const node = try self.state.arena.create(Node.FunctionDeclaration);
        node.* = .{
            .name = name,
            .parameters = try self.state.arena.dupe(Token, params.items),
            .body = try self.state.arena.dupe(*Node, body.items),
            .is_local = is_local,
        };
        return &node.base;
    }

    // funcstat -> FUNCTION funcname body
    fn funcstat(self: *Self) Error!*Node {
        std.debug.assert(self.state.token.id == .keyword_function);
        const function_token = self.state.token;
        try self.nextToken();

        const name = try self.funcname();
        return self.funcbody(function_token, name, false);
    }

    // funcname -> NAME {field} [`:' NAME]
    fn funcname(self: *Self) Error!*Node {
        const name_token = try self.checkname();
        var identifier_node = try self.state.arena.create(Node.Identifier);
        identifier_node.* = .{
            .token = name_token,
        };
        var node = &identifier_node.base;

        while (self.state.token.isChar('.')) {
            const separator = self.state.token;
            try self.nextToken(); // skip separator
            const field_token = try self.checkname();

            var new_node = try self.state.arena.create(Node.FieldAccess);
            new_node.* = .{
                .prefix = node,
                .field = field_token,
                .separator = separator,
            };
            node = &new_node.base;
        }
        if (self.state.token.isChar(':')) {
            const separator = self.state.token;
            try self.nextToken();
            const field_token = try self.checkname();

            var new_node = try self.state.arena.create(Node.FieldAccess);
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
        std.debug.assert(self.state.token.id == .keyword_return);
        try self.nextToken();

        var return_values = std.ArrayList(*Node).init(self.state.allocator);
        defer return_values.deinit();

        const no_return_values = blockFollow(self.state.token) or (self.state.token.isChar(';'));
        if (!no_return_values) {
            _ = try self.explist1(&return_values);
        }

        const node = try self.state.arena.create(Node.ReturnStatement);
        node.* = .{
            .values = try self.state.arena.dupe(*Node, return_values.items),
        };
        return &node.base;
    }

    /// cond -> exp
    fn cond(self: *Self) Error!*Node {
        return self.expr();
    }

    /// whilestat -> WHILE cond DO block END
    fn whilestat(self: *Self) Error!*Node {
        std.debug.assert(self.state.token.id == .keyword_while);
        const while_token = self.state.token;

        try self.nextToken();

        const condition = try self.cond();

        try self.checknext(.keyword_do);

        var body = std.ArrayList(*Node).init(self.state.allocator);
        defer body.deinit();

        const in_loop_prev = self.state.in_loop;
        self.state.in_loop = true;
        try self.block(&body);
        self.state.in_loop = in_loop_prev;

        try self.check_match(.keyword_end, while_token);

        var while_statement = try self.state.arena.create(Node.WhileStatement);
        while_statement.* = .{
            .condition = condition,
            .body = try self.state.arena.dupe(*Node, body.items),
        };
        return &while_statement.base;
    }

    fn breakstat(self: *Self) Error!*Node {
        std.debug.assert(self.state.token.id == .keyword_break);
        const break_token = self.state.token;
        try self.nextToken();

        if (!self.state.in_loop) {
            return self.reportParseError(ParseError.NoLoopToBreak);
        }

        var break_statement = try self.state.arena.create(Node.BreakStatement);
        break_statement.* = .{
            .token = break_token,
        };
        return &break_statement.base;
    }

    fn dostat(self: *Self) Error!*Node {
        std.debug.assert(self.state.token.id == .keyword_do);
        const do_token = self.state.token;
        try self.nextToken();

        var body = std.ArrayList(*Node).init(self.state.allocator);
        defer body.deinit();

        try self.block(&body);

        try self.check_match(.keyword_end, do_token);

        const node = try self.state.arena.create(Node.DoStatement);
        node.* = .{
            .body = try self.state.arena.dupe(*Node, body.items),
        };
        return &node.base;
    }

    /// repeatstat -> REPEAT block UNTIL cond
    fn repeatstat(self: *Self) Error!*Node {
        std.debug.assert(self.state.token.id == .keyword_repeat);
        const repeat_token = self.state.token;
        try self.nextToken();

        var body = std.ArrayList(*Node).init(self.state.allocator);
        defer body.deinit();

        const in_loop_prev = self.state.in_loop;
        self.state.in_loop = true;
        try self.block(&body);
        self.state.in_loop = in_loop_prev;

        try self.check_match(.keyword_until, repeat_token);

        const condition = try self.cond();

        const node = try self.state.arena.create(Node.RepeatStatement);
        node.* = .{
            .condition = condition,
            .body = try self.state.arena.dupe(*Node, body.items),
        };
        return &node.base;
    }

    /// forstat -> FOR (fornum | forlist) END
    fn forstat(self: *Self) Error!*Node {
        std.debug.assert(self.state.token.id == .keyword_for);
        const for_token = self.state.token;

        try self.nextToken();

        const name_token = try self.checkname();

        var for_node: *Node = undefined;

        switch (self.state.token.id) {
            .single_char => switch (self.state.token.char.?) {
                '=' => for_node = try self.fornum(name_token),
                ',' => for_node = try self.forlist(name_token),
                else => return self.reportParseError(ParseError.ExpectedEqualsOrIn),
            },
            .keyword_in => for_node = try self.forlist(name_token),
            else => return self.reportParseError(ParseError.ExpectedEqualsOrIn),
        }

        try self.check_match(.keyword_end, for_token);

        return for_node;
    }

    /// fornum -> NAME = exp1,exp1[,exp1] forbody
    fn fornum(self: *Self, name_token: Token) Error!*Node {
        try self.checkcharnext('=');

        const start_expression = try self.expr();

        try self.checkcharnext(',');

        const end_expression = try self.expr();

        var increment_expression: ?*Node = null;
        if (try self.testcharnext(',')) {
            increment_expression = try self.expr();
        }

        try self.checknext(.keyword_do);

        var body = std.ArrayList(*Node).init(self.state.allocator);
        defer body.deinit();

        const in_loop_prev = self.state.in_loop;
        self.state.in_loop = true;
        try self.block(&body);
        self.state.in_loop = in_loop_prev;

        var for_node = try self.state.arena.create(Node.ForStatementNumeric);
        for_node.* = .{
            .name = name_token,
            .start = start_expression,
            .end = end_expression,
            .increment = increment_expression,
            .body = try self.state.arena.dupe(*Node, body.items),
        };
        return &for_node.base;
    }

    /// forlist -> NAME {,NAME} IN explist1 forbody
    fn forlist(self: *Self, first_name_token: Token) Error!*Node {
        var names = try std.ArrayList(Token).initCapacity(self.state.allocator, 1);
        defer names.deinit();

        names.appendAssumeCapacity(first_name_token);

        while (try self.testcharnext(',')) {
            const name_token = try self.checkname();
            try names.append(name_token);
        }
        try self.checknext(.keyword_in);

        var expressions = std.ArrayList(*Node).init(self.state.allocator);
        defer expressions.deinit();

        _ = try self.explist1(&expressions);

        try self.checknext(.keyword_do);

        var body = std.ArrayList(*Node).init(self.state.allocator);
        defer body.deinit();

        const in_loop_prev = self.state.in_loop;
        self.state.in_loop = true;
        try self.block(&body);
        self.state.in_loop = in_loop_prev;

        var for_node = try self.state.arena.create(Node.ForStatementGeneric);
        for_node.* = .{
            .names = try self.state.arena.dupe(Token, names.items),
            .expressions = try self.state.arena.dupe(*Node, expressions.items),
            .body = try self.state.arena.dupe(*Node, body.items),
        };
        return &for_node.base;
    }

    /// sort of the equivalent of Lua's test_then_block in lparser.c but handles else too
    fn ifclause(self: *Self) Error!*Node {
        std.debug.assert(self.state.token.id == .keyword_if or self.state.token.id == .keyword_elseif or self.state.token.id == .keyword_else);

        const if_token = self.state.token;
        var condition: ?*Node = null;

        try self.nextToken();
        switch (if_token.id) {
            .keyword_if, .keyword_elseif => {
                condition = try self.cond();
                try self.checknext(.keyword_then);
            },
            .keyword_else => {},
            else => unreachable,
        }

        var body = std.ArrayList(*Node).init(self.state.allocator);
        defer body.deinit();

        try self.block(&body);

        var if_clause = try self.state.arena.create(Node.IfClause);
        if_clause.* = .{
            .if_token = if_token,
            .condition = condition,
            .body = try self.state.arena.dupe(*Node, body.items),
        };
        return &if_clause.base;
    }

    /// ifstat -> IF cond THEN block {ELSEIF cond THEN block} [ELSE block] END
    fn ifstat(self: *Self) Error!*Node {
        std.debug.assert(self.state.token.id == .keyword_if);
        const if_token = self.state.token;

        var clauses = std.ArrayList(*Node).init(self.state.allocator);
        defer clauses.deinit();

        // if
        const if_clause = try self.ifclause();
        try clauses.append(if_clause);

        // elseif
        while (self.state.token.id == .keyword_elseif) {
            const elseif_clause = try self.ifclause();
            try clauses.append(elseif_clause);
        }

        // else
        if (self.state.token.id == .keyword_else) {
            const else_clause = try self.ifclause();
            try clauses.append(else_clause);
        }

        try self.check_match(.keyword_end, if_token);

        var if_statement = try self.state.arena.create(Node.IfStatement);
        if_statement.* = .{
            .clauses = try self.state.arena.dupe(*Node, clauses.items),
        };
        return &if_statement.base;
    }

    fn localfunc(self: *Self, function_token: Token) Error!*Node {
        const name_token = try self.checkname();
        var name = try self.state.arena.create(Node.Identifier);
        name.* = .{ .token = name_token };
        return self.funcbody(function_token, &name.base, true);
    }

    /// listfield -> expr
    fn listfield(self: *Self) Error!*Node {
        var value = try self.expr();
        var node = try self.state.arena.create(Node.TableField);
        node.* = .{
            .key = null,
            .value = value,
        };
        return &node.base;
    }

    /// recfield -> (NAME | `['exp1`]') = exp1
    fn recfield(self: *Self) Error!*Node {
        var key: *Node = get_key: {
            if (self.state.token.id == .name) {
                const name_token = try self.checkname();
                // This might be kinda weird, but the name token here is actually used as
                // more of a string literal, so create a Literal node instead of Identifier.
                // This is a special case.
                // TODO revisit this?
                var name_node = try self.state.arena.create(Node.Literal);
                name_node.* = .{ .token = name_token };
                break :get_key &name_node.base;
            } else {
                std.debug.assert(self.state.token.isChar('['));
                try self.nextToken(); // skip the [

                const key_expr = try self.expr();

                try self.checkcharnext(']');

                break :get_key key_expr;
            }
        };
        try self.checkcharnext('=');

        const value = try self.expr();

        var node = try self.state.arena.create(Node.TableField);
        node.* = .{
            .key = key,
            .value = value,
        };
        return &node.base;
    }

    /// constructor -> ??
    fn constructor(self: *Self) Error!*Node {
        const open_brace_token = self.state.token;
        try self.checkcharnext('{');

        var fields = std.ArrayList(*Node).init(self.state.allocator);
        defer fields.deinit();

        while (!self.state.token.isChar('}')) {
            var field: *Node = get_field: {
                switch (self.state.token.id) {
                    .name => {
                        // TODO presumably should catch EOF or errors here and translate them to more appropriate errors
                        const lookahead_token = self.lexer.lookahead() catch |err| {
                            break :get_field try self.listfield();
                        };
                        if (lookahead_token.isChar('=')) {
                            break :get_field try self.recfield();
                        } else {
                            break :get_field try self.listfield();
                        }
                    },
                    .single_char => switch (self.state.token.char.?) {
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

        const close_brace_token = self.state.token;
        try self.checkchar_match('}', open_brace_token);

        var node = try self.state.arena.create(Node.TableConstructor);
        node.* = .{
            .fields = try self.state.arena.dupe(*Node, fields.items),
            .open_token = open_brace_token,
            .close_token = close_brace_token,
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

        var variables = std.ArrayList(*Node).init(self.state.allocator);
        defer variables.deinit();

        var values = std.ArrayList(*Node).init(self.state.allocator);
        defer values.deinit();

        if (is_local) {
            while (true) {
                const name_token = try self.checkname();
                var identifier = try self.state.arena.create(Node.Identifier);
                identifier.* = .{ .token = name_token };
                try variables.append(&identifier.base);

                // TODO this needs work, it doesn't really belong here.
                // We need to keep track of *all* local vars in order to function
                // like the Lua parser (even local var literals like `self`).
                // Might need to be checked at compile-time rather than parse-time.
                if (variables.items.len > max_local_vars) {
                    return self.reportParseError(ParseError.TooManyLocalVariables);
                }

                if (!try self.testcharnext(',')) break;
            }
            if (try self.testcharnext('=')) {
                _ = try self.explist1(&values);
            }
        } else {
            var variable = first_variable.?;
            while (true) {
                if (!variable.can_be_assigned_to) {
                    return self.reportParseError(ParseError.SyntaxError);
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

        var local = try self.state.arena.create(Node.AssignmentStatement);
        local.* = .{
            .variables = try self.state.arena.dupe(*Node, variables.items),
            .values = try self.state.arena.dupe(*Node, values.items),
            .is_local = is_local,
        };
        return &local.base;
    }

    /// stat -> func | assignment
    fn exprstat(self: *Self) Error!*Node {
        var expression = try self.primaryexp();
        if (expression.node.id == .call) {
            const call_node = @fieldParentPtr(Node.Call, "base", expression.node);
            call_node.is_statement = true;
        } else {
            // if it's not a call, then it's an assignment
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
        switch (self.state.token.id) {
            .string, .number, .keyword_false, .keyword_true, .keyword_nil, .ellipsis => {
                if (self.state.token.id == .ellipsis and !self.state.in_vararg_func) {
                    return self.reportParseError(ParseError.VarArgOutsideVarArgFunction);
                }
                const node = try self.state.arena.create(Node.Literal);
                node.* = .{
                    .token = self.state.token,
                };
                try self.nextToken();
                return &node.base;
            },
            .single_char => {
                switch (self.state.token.char.?) {
                    '{' => return self.constructor(),
                    else => {},
                }
            },
            .keyword_function => {
                const function_token = self.state.token;
                try self.nextToken(); // skip function
                return self.funcbody(function_token, null, false);
            },
            else => {},
        }
        const expression = try self.primaryexp();
        return expression.node;
    }

    pub const SubExpr = struct {
        node: *Node,
        untreated_operator: ?Token,
    };

    /// subexpr -> (simpleexp | unop subexpr) { binop subexpr }
    /// where `binop' is any binary operator with a priority higher than `limit'
    fn subexpr(self: *Self, limit: usize) Error!SubExpr {
        try self.enterlevel();
        var node: *Node = get_node: {
            if (isUnaryOperator(self.state.token)) {
                const unary_token = self.state.token;
                try self.nextToken();
                var argument_expr = try self.subexpr(unary_priority);

                const unary_node = try self.state.arena.create(Node.UnaryExpression);
                unary_node.* = .{
                    .operator = unary_token,
                    .argument = argument_expr.node,
                };
                break :get_node &unary_node.base;
            } else {
                break :get_node try self.simpleexp();
            }
        };
        var op: ?Token = self.state.token;
        while (op != null and isBinaryOperator(op.?)) {
            const binary_token = op.?;
            const priority = getBinaryPriority(binary_token);
            if (priority.left <= limit) break;

            try self.nextToken();
            var right_expr = try self.subexpr(priority.right);

            const new_node = try self.state.arena.create(Node.BinaryExpression);
            new_node.* = .{
                .operator = binary_token,
                .left = node,
                .right = right_expr.node,
            };
            node = &new_node.base;

            op = right_expr.untreated_operator;
        } else {
            op = null;
        }
        self.leavelevel();
        return SubExpr{
            .node = node,
            .untreated_operator = op,
        };
    }

    fn expr(self: *Self) Error!*Node {
        return (try self.subexpr(0)).node;
    }

    fn primaryexp(self: *Self) Error!PossibleLValueExpression {
        var arguments = std.ArrayList(*Node).init(self.state.allocator);
        defer arguments.deinit();

        var expression = try self.prefixexp();
        var is_func_call = false;
        loop: while (true) {
            switch (self.state.token.id) {
                .single_char => {
                    switch (self.state.token.char.?) {
                        '.' => {
                            const separator = self.state.token;
                            try self.nextToken(); // skip the dot
                            const field_token = try self.checkname();

                            var new_node = try self.state.arena.create(Node.FieldAccess);
                            new_node.* = .{
                                .prefix = expression.node,
                                .field = field_token,
                                .separator = separator,
                            };
                            expression.node = &new_node.base;
                            expression.can_be_assigned_to = true;
                        },
                        '[' => {
                            const open_token = self.state.token;
                            try self.nextToken(); // skip the [
                            const index = try self.expr();
                            const close_token = self.state.token;
                            try self.checkcharnext(']');

                            var new_node = try self.state.arena.create(Node.IndexAccess);
                            new_node.* = .{
                                .prefix = expression.node,
                                .index = index,
                                .open_token = open_token,
                                .close_token = close_token,
                            };
                            expression.node = &new_node.base;
                            expression.can_be_assigned_to = true;
                        },
                        ':' => {
                            const separator = self.state.token;
                            try self.nextToken(); // skip the :
                            const field_token = try self.checkname();

                            var new_node = try self.state.arena.create(Node.FieldAccess);
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
        var arguments = std.ArrayList(*Node).init(self.state.allocator);
        defer arguments.deinit();

        var open_args_token: ?Token = null;
        var close_args_token: ?Token = null;
        switch (self.state.token.id) {
            .single_char => switch (self.state.token.char.?) {
                '(' => {
                    open_args_token = self.state.token;
                    const last_token = expression.getLastToken();
                    if (last_token.line_number != open_args_token.?.line_number) {
                        return self.reportParseError(ParseError.AmbiguousSyntax);
                    }
                    try self.nextToken();
                    const has_no_arguments = self.state.token.isChar(')');
                    if (!has_no_arguments) {
                        _ = try self.explist1(&arguments);
                    }
                    close_args_token = self.state.token;
                    try self.checkchar_match(')', open_args_token.?);
                },
                '{' => {
                    const node = try self.constructor();
                    try arguments.append(node);
                },
                else => {
                    return self.reportParseError(ParseError.FunctionArgumentsExpected);
                },
            },
            .string => {
                const node = try self.state.arena.create(Node.Literal);
                node.* = .{
                    .token = self.state.token,
                };
                try arguments.append(&node.base);
                try self.nextToken();
            },
            else => return self.reportParseError(ParseError.FunctionArgumentsExpected),
        }

        var call = try self.state.arena.create(Node.Call);
        call.* = .{
            .expression = expression,
            .arguments = try self.state.arena.dupe(*Node, arguments.items),
            .open_args_token = open_args_token,
            .close_args_token = close_args_token,
        };
        return &call.base;
    }

    fn prefixexp(self: *Self) Error!PossibleLValueExpression {
        switch (self.state.token.id) {
            .name => {
                const node = try self.state.arena.create(Node.Identifier);
                node.* = .{
                    .token = self.state.token,
                };
                try self.nextToken();
                return PossibleLValueExpression{ .node = &node.base };
            },
            .single_char => switch (self.state.token.char.?) {
                '(' => {
                    const open_paren_token = self.state.token;

                    try self.nextToken();
                    const expression = try self.expr();

                    const node = try self.state.arena.create(Node.GroupedExpression);
                    node.* = .{
                        .open_token = open_paren_token,
                        .expression = expression,
                        .close_token = self.state.token,
                    };

                    try self.checkchar_match(')', open_paren_token);

                    return PossibleLValueExpression{
                        .node = &node.base,
                        .can_be_assigned_to = false,
                    };
                },
                else => {},
            },
            else => {},
        }
        return self.reportParseError(ParseError.UnexpectedSymbol);
    }

    fn nextToken(self: *Self) Lexer.Error!void {
        self.state.token = try self.lexer.next();
    }

    /// Skip to next token if the current token matches the given ID
    fn testnext(self: *Self, id: Token.Id) !bool {
        if (self.state.token.id == id) {
            try self.nextToken();
            return true;
        }
        return false;
    }

    /// Skip to next token if the current token is a single character token with the given value
    fn testcharnext(self: *Self, char: u8) !bool {
        if (self.state.token.isChar(char)) {
            try self.nextToken();
            return true;
        }
        return false;
    }

    fn check(self: *Self, expected_id: Token.Id) !void {
        if (self.state.token.id != expected_id) {
            return self.reportParseErrorExpected(ParseError.ExpectedDifferentToken, Token{
                .id = expected_id,
                .start = self.state.token.start,
                .end = self.state.token.end,
                .char = null,
                .line_number = self.state.token.line_number,
            });
        }
    }

    fn checkchar(self: *Self, expected_char: u8) !void {
        if (!self.state.token.isChar(expected_char)) {
            return self.reportParseErrorExpected(ParseError.ExpectedDifferentToken, Token{
                .id = .single_char,
                .start = self.state.token.start,
                .end = self.state.token.end,
                .char = expected_char,
                .line_number = self.state.token.line_number,
            });
        }
    }

    fn checknext(self: *Self, expected_id: Token.Id) !void {
        try self.check(expected_id);
        try self.nextToken();
    }

    fn checkcharnext(self: *Self, expected_char: u8) !void {
        try self.checkchar(expected_char);
        try self.nextToken();
    }

    // TODO eliminate?
    /// Returns the name token, since it is typically used when constructing the AST node
    fn checkname(self: *Self) !Token {
        const token = self.state.token;
        try self.checknext(.name);
        return token;
    }

    fn check_match(self: *Self, expected_id: Token.Id, to_close: Token) !void {
        if (!try self.testnext(expected_id)) {
            return self.reportParseErrorExpectedToMatch(ParseError.ExpectedDifferentToken, Token{
                .id = expected_id,
                .start = self.state.token.start,
                .end = self.state.token.end,
                .char = null,
                .line_number = self.state.token.line_number,
            }, to_close);
        }
    }

    fn checkchar_match(self: *Self, expected_char: u8, to_close: Token) !void {
        if (!try self.testcharnext(expected_char)) {
            return self.reportParseErrorExpectedToMatch(ParseError.ExpectedDifferentToken, Token{
                .id = .single_char,
                .start = self.state.token.start,
                .end = self.state.token.end,
                .char = expected_char,
                .line_number = self.state.token.line_number,
            }, to_close);
        }
    }

    fn enterlevel(self: *Self) !void {
        self.state.syntax_level += 1;
        if (self.state.syntax_level > max_syntax_levels) {
            return self.reportParseError(ParseError.TooManySyntaxLevels);
        }
    }

    fn leavelevel(self: *Self) void {
        std.debug.assert(self.state.syntax_level > 0);
        self.state.syntax_level -= 1;
    }

    fn reportParseError(self: *Self, err: ParseError) ParseError {
        return self.reportParseErrorExpectedToMatch(err, null, null);
    }

    fn reportParseErrorExpected(self: *Self, err: ParseError, expected: ?Token) ParseError {
        return self.reportParseErrorExpectedToMatch(err, expected, null);
    }

    fn reportParseErrorExpectedToMatch(self: *Self, err: ParseError, expected: ?Token, expected_match: ?Token) ParseError {
        self.error_context = .{
            .token = self.state.token,
            .expected = expected,
            .expected_match = expected_match,
            .err = err,
        };
        return err;
    }

    pub fn renderErrorAlloc(self: *Self, allocator: *Allocator) ![]const u8 {
        if (self.error_context) |*ctx| {
            return ctx.renderAlloc(allocator, self);
        } else {
            return self.lexer.renderErrorAlloc(allocator);
        }
    }
};

pub const unary_priority = 8;

// TODO: move to Token?
fn blockFollow(token: Token) bool {
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

// TODO: move to Token?
fn isUnaryOperator(token: Token) bool {
    return switch (token.id) {
        .keyword_not => true,
        .single_char => switch (token.char.?) {
            '-', '#' => true,
            else => false,
        },
        else => false,
    };
}

// TODO: move to Token?
fn isBinaryOperator(token: Token) bool {
    return switch (token.id) {
        .concat, .ne, .eq, .le, .ge, .keyword_and, .keyword_or => true,
        .single_char => switch (token.char.?) {
            '+', '-', '*', '/', '%', '^', '<', '>' => true,
            else => false,
        },
        else => false,
    };
}

pub const BinaryPriority = struct {
    left: u8,
    right: u8,
};

// TODO: move to Token?
fn getBinaryPriority(token: Token) BinaryPriority {
    return switch (token.id) {
        .single_char => switch (token.char.?) {
            '+', '-' => BinaryPriority{ .left = 6, .right = 6 },
            '*', '/', '%' => BinaryPriority{ .left = 7, .right = 7 },
            '^' => BinaryPriority{ .left = 10, .right = 9 },
            '>', '<' => BinaryPriority{ .left = 3, .right = 3 },
            else => unreachable,
        },
        .concat => BinaryPriority{ .left = 5, .right = 4 },
        .eq, .ne, .le, .ge => BinaryPriority{ .left = 3, .right = 3 },
        .keyword_and => BinaryPriority{ .left = 2, .right = 2 },
        .keyword_or => BinaryPriority{ .left = 1, .right = 1 },
        else => unreachable,
    };
}

test "check hello world ast" {
    const allocator = std.testing.allocator;
    const source = "print \"hello world\"";
    var lexer = Lexer.init(source, source);
    var parser = Parser.init(&lexer);
    var tree = try parser.parse(std.testing.allocator);
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
    var lexer = Lexer.init(source, source);
    var parser = Parser.init(&lexer);
    var tree = try parser.parse(allocator);
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
    // long string
    try testParse("a[[string]]",
        \\chunk
        \\ call
        \\  identifier
        \\  (
        \\   literal <string>
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
    try testParse("for i=1,2 do break end",
        \\chunk
        \\ for_statement_numeric
        \\  literal <number>
        \\  literal <number>
        \\ do
        \\  break_statement
        \\
    );
    // break just needs to be at the end of its immediate block, so wrapping it in a `do end`
    // allows you to put a break statement before the end of another block
    try testParse("for i=1,2 do do break end print(\"dead\") end",
        \\chunk
        \\ for_statement_numeric
        \\  literal <number>
        \\  literal <number>
        \\ do
        \\  do_statement
        \\   break_statement
        \\  call
        \\   identifier
        \\   (
        \\    literal <string>
        \\   )
        \\
    );
    // break in loop with nested loop
    try testParse(
        \\for i=1,2 do
        \\  for j=1,2 do
        \\  end
        \\  break
        \\end
    ,
        \\chunk
        \\ for_statement_numeric
        \\  literal <number>
        \\  literal <number>
        \\ do
        \\  for_statement_numeric
        \\   literal <number>
        \\   literal <number>
        \\  do
        \\  break_statement
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
        \\   grouped_expression
        \\   (
        \\    identifier
        \\   )
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
    try testParse("a = ...",
        \\chunk
        \\ assignment_statement
        \\  identifier
        \\ =
        \\  literal ...
        \\
    );
}

test "assignment errors" {
    expectParseError(ParseError.SyntaxError, "(a) = nil");
    expectParseError(ParseError.UnexpectedSymbol, "(a)() = nil");
    expectParseError(ParseError.FunctionArgumentsExpected, "a:b = nil");
    expectParseError(ParseError.SyntaxError, "(a)");
    expectParseError(ParseError.UnexpectedSymbol, "true = nil");
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

test "operators" {
    try testParse("a = #b",
        \\chunk
        \\ assignment_statement
        \\  identifier
        \\ =
        \\  unary_expression #
        \\   identifier
        \\
    );
    try testParse("a = ###b",
        \\chunk
        \\ assignment_statement
        \\  identifier
        \\ =
        \\  unary_expression #
        \\   unary_expression #
        \\    unary_expression #
        \\     identifier
        \\
    );
    try testParse("a = a+i < b/2+1",
        \\chunk
        \\ assignment_statement
        \\  identifier
        \\ =
        \\  binary_expression <
        \\   binary_expression +
        \\    identifier
        \\    identifier
        \\   binary_expression +
        \\    binary_expression /
        \\     identifier
        \\     literal <number>
        \\    literal <number>
        \\
    );
    try testParse("a = 5+x^2*8",
        \\chunk
        \\ assignment_statement
        \\  identifier
        \\ =
        \\  binary_expression +
        \\   literal <number>
        \\   binary_expression *
        \\    binary_expression ^
        \\     identifier
        \\     literal <number>
        \\    literal <number>
        \\
    );
    try testParse("a = a < y and y <= z",
        \\chunk
        \\ assignment_statement
        \\  identifier
        \\ =
        \\  binary_expression and
        \\   binary_expression <
        \\    identifier
        \\    identifier
        \\   binary_expression <=
        \\    identifier
        \\    identifier
        \\
    );
    try testParse("a = -x^2",
        \\chunk
        \\ assignment_statement
        \\  identifier
        \\ =
        \\  unary_expression -
        \\   binary_expression ^
        \\    identifier
        \\    literal <number>
        \\
    );
    try testParse("a = x^y^z",
        \\chunk
        \\ assignment_statement
        \\  identifier
        \\ =
        \\  binary_expression ^
        \\   identifier
        \\   binary_expression ^
        \\    identifier
        \\    identifier
        \\
    );
}

test "errors" {
    expectParseError(ParseError.ExpectedDifferentToken, "untilû");
    expectParseError(ParseError.ExpectedDifferentToken, "until");

    // return and break must be the last statement in a block
    expectParseError(ParseError.ExpectedDifferentToken, "return; local a = 1");
    expectParseError(ParseError.ExpectedDifferentToken, "for i=1,2 do break; local a = 1 end");

    // break must be in a loop
    expectParseError(ParseError.NoLoopToBreak, "break");

    expectParseError(ParseError.ExpectedDifferentToken,
        \\local a = function()
        \\
        \\
    );

    expectParseError(ParseError.ExpectedDifferentToken,
        \\(
        \\
        \\a
        \\
    );

    expectParseError(ParseError.AmbiguousSyntax, "f\n()");
    expectParseError(ParseError.AmbiguousSyntax, "(\nf\n)\n()");

    expectParseError(ParseError.VarArgOutsideVarArgFunction, "function a() local b = {...} end");
}
