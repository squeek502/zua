const std = @import("std");
const Allocator = std.mem.Allocator;
const zua = @import("zua.zig");
const Instruction = zua.opcodes.Instruction;
const InstructionABC = zua.opcodes.InstructionABC;
const InstructionABx = zua.opcodes.InstructionABx;
const Node = zua.ast.Node;
const Function = zua.object.Function;
const Constant = zua.object.Constant;
const Lexer = zua.lex.Lexer;
const Parser = zua.parse.Parser;
const max_stack_size = zua.parse.max_stack_size;
const OpCode = zua.opcodes.OpCode;

pub fn compile(allocator: *Allocator, source: []const u8) !Function {
    var lexer = Lexer.init(source, source);
    var parser = Parser.init(&lexer);
    var tree = try parser.parse(allocator);
    defer tree.deinit();

    var arena_allocator = std.heap.ArenaAllocator.init(allocator);
    defer arena_allocator.deinit();
    const arena = &arena_allocator.allocator;

    var compiler = Compiler{
        .source = source,
        .arena = arena,
        .allocator = allocator,
        .func = undefined,
    };
    defer compiler.deinit();

    const main_func = try compiler.genChunk(tree.chunk());

    return Function{
        .name = "",
        .code = try allocator.dupe(Instruction, main_func.code.items),
        .constants = try allocator.dupe(Constant, main_func.constants.items),
        .allocator = allocator,
        .max_stack_size = main_func.max_stack_size,
        .varargs = main_func.varargs,
    };
}

pub const Compiler = struct {
    source: []const u8,
    arena: *Allocator,
    allocator: *Allocator,
    func: *Func,

    pub const Error = error{CompileError} || Allocator.Error;

    pub fn deinit(self: *Compiler) void {}

    /// State for an incomplete/to-be-compiled function
    /// Analogous to FuncState in PUC Lua
    pub const Func = struct {
        max_stack_size: u8 = 2, // registers 0/1 are always valid
        free_register: u8 = 0, // TODO what should this type actually be?
        cur_exp: ExpDesc = .{ .kind = .@"void" },
        code: std.ArrayList(Instruction),
        constants: std.ArrayList(Constant),
        constants_map: Constant.Map,
        varargs: Function.VarArgs,
        prev: ?*Func,
        nactvar: u8 = 0, // TODO what is this?

        pub fn checkstack(self: *Func, n: u8) !void {
            const newstack = self.free_register + n;
            if (newstack > self.max_stack_size) {
                if (newstack >= max_stack_size) {
                    @panic("TODO function or expression too complex");
                }
                self.max_stack_size = newstack;
            }
        }

        pub fn reserveregs(self: *Func, n: u8) !void {
            try self.checkstack(n);
            self.free_register += n;
        }

        pub fn exp2nextreg(self: *Func, e: *ExpDesc) !void {
            try self.dischargevars(e);
            try self.freeexp(e);
            try self.reserveregs(1);
            try self.exp2reg(e, self.free_register - 1);
        }

        pub fn exp2anyreg(self: *Func, e: *ExpDesc) !u8 {
            try self.dischargevars(e);
            if (e.kind == .nonreloc) {
                // exp is already in a register
                if (!e.hasjumps()) return e.value.?.reg;
                // reg is not a local?
                if (e.value.?.reg >= self.nactvar) {
                    try self.exp2reg(e, e.value.?.reg);
                    return e.value.?.reg;
                }
            }

            try self.exp2nextreg(e);
            return e.value.?.reg;
        }

        pub fn dischargevars(self: *Func, e: *ExpDesc) !void {
            switch (e.kind) {
                .local_register => {
                    e.kind = .nonreloc;
                },
                .upvalue => {
                    //const index = try self.emitABC(.getupval, 0, @intCast(u18, e.val.?), 0);
                    //e.val = @intCast(isize, index);
                    e.kind = .relocable;
                    @panic("TODO");
                },
                .global => {
                    const index = try self.emitABx(.getglobal, 0, @intCast(u18, e.value.?.index));
                    e.value = .{ .index = index };
                    e.kind = .relocable;
                },
                .indexed => {
                    @panic("TODO");
                },
                .vararg, .call => {
                    try self.setoneret(e);
                },
                else => {}, // there is one value available (somewhere)
            }
        }

        pub fn setoneret(self: *Func, e: *ExpDesc) !void {
            if (e.kind == .call) {
                e.kind = .nonreloc;
                const instruction = self.getcode(e);
                e.value = .{ .reg = instruction.a };
            } else if (e.kind == .vararg) {
                const instruction = self.getcode(e);
                const instructionABC = @ptrCast(*InstructionABC, instruction);
                instructionABC.*.b = 2;
                e.kind = .relocable;
            }
        }

        pub fn freeexp(self: *Func, e: *ExpDesc) !void {
            if (e.kind == .nonreloc) {
                self.freereg(e.value.?.reg);
            }
        }

        pub fn freereg(self: *Func, reg: u8) void {
            if (!zua.opcodes.isConstant(reg) and reg >= self.nactvar) {
                self.free_register -= 1;
                std.debug.assert(reg == self.free_register);
            }
        }

        pub fn getcode(self: *Func, e: *ExpDesc) *Instruction {
            // TODO some sanity checking
            const index = e.value.?.index;
            return &self.code.items[index];
        }

        pub fn exp2reg(self: *Func, e: *ExpDesc, reg: u8) !void {
            try self.discharge2reg(e, reg);
            if (e.kind == .jmp) {
                //self.concat(...)
                @panic("TODO");
            }
            if (e.hasjumps()) {
                @panic("TODO");
            }

            e.patch_list_exit_when_true = null;
            e.patch_list_exit_when_false = null;
            e.value = .{ .reg = reg };
            e.kind = .nonreloc;
        }

        pub fn discharge2reg(self: *Func, e: *ExpDesc, reg: u8) !void {
            try self.dischargevars(e);
            switch (e.kind) {
                .nil => {
                    _ = try self.emitABC(.loadnil, reg, 1, 0);
                },
                .@"false", .@"true" => {
                    _ = try self.emitABC(.loadbool, reg, @boolToInt(e.kind == .@"true"), 0);
                },
                .index_of_constant => {
                    _ = try self.emitABx(.loadk, reg, @intCast(u18, e.value.?.index));
                },
                .number => {
                    // Need to add number constants to the constant table here,
                    // because not every constant is needed in the final bytecode,
                    // i.e. `return 1 + 2` should be resolved to only need the
                    // constant `3` (1 and 2 are not in the final bytecode)
                    const index = try self.putConstant(Constant{ .number = e.value.?.number });
                    _ = try self.emitABx(.loadk, reg, @intCast(u18, index));
                },
                .relocable => {
                    const instruction = self.getcode(e);
                    const instructionABC = @ptrCast(*InstructionABC, instruction);
                    instructionABC.*.a = reg;
                },
                .nonreloc => {
                    if (reg != e.value.?.reg) {
                        //_ = try self.emitABC(.move, reg, e.val.?, 0);
                    }
                },
                .@"void", .jmp => return, // nothing to do
                else => unreachable,
            }
            e.value = .{ .reg = reg };
            e.kind = .nonreloc;
        }

        // TODO better name for `first` param
        /// luaK_ret equivalent
        pub fn emitReturn(self: *Func, first: u8, num_returns: u9) !usize {
            return self.emitABC(.@"return", first, num_returns + 1, 0);
        }

        /// Appends a new instruction to the Func's code and returns the
        /// index of the added instruction
        /// luaK_codeABC equivalent
        pub fn emitABC(self: *Func, op: OpCode, a: u8, b: u9, c: u9) !usize {
            try self.code.append(@bitCast(Instruction, InstructionABC.init(
                op,
                a,
                b,
                c,
            )));
            return self.code.items.len - 1;
        }

        /// Appends a new instruction to the Func's code and returns the
        /// index of the added instruction
        /// luaK_codeABx equivalent
        pub fn emitABx(self: *Func, op: OpCode, a: u8, bx: u18) !usize {
            try self.code.append(@bitCast(Instruction, InstructionABx.init(
                op,
                a,
                bx,
            )));
            return self.code.items.len - 1;
        }

        pub fn putConstant(self: *Func, constant: Constant) Error!usize {
            const result = try self.constants_map.getOrPut(constant);
            if (result.found_existing) {
                return result.entry.value;
            } else {
                result.entry.value = self.constants.items.len;
                try self.constants.append(constant);
                return result.entry.value;
            }
        }
    };

    pub const ExpDesc = struct {
        kind: Kind,
        value: ?Value = null,
        patch_list_exit_when_true: ?i32 = null,
        patch_list_exit_when_false: ?i32 = null,

        pub const Value = union {
            index: usize,
            number: f64,
            reg: u8,
        };

        pub const Kind = enum {
            @"void", // no value
            nil,
            @"true",
            @"false",
            index_of_constant, // info = index of constant in `k'
            number, // nval = numerical value
            local_register, // info = local register
            upvalue, // info = index of upvalue in `upvalues'
            global, // info = index of table; aux = index of global name in `k'
            indexed, // info = table register; aux = index register (or `k')
            jmp, // info = instruction pc
            relocable, // info = instruction pc
            nonreloc, // info = result register
            call, // info = instruction pc
            vararg, // info = instruction pc
        };

        pub fn hasjumps(self: *ExpDesc) bool {
            // TODO
            return false;
        }
    };

    pub fn genChunk(self: *Compiler, chunk: *Node.Chunk) Error!*Func {
        var main_func: *Func = try self.arena.create(Func);
        main_func.* = .{
            .code = std.ArrayList(Instruction).init(self.arena),
            .constants = std.ArrayList(Constant).init(self.arena),
            .constants_map = Constant.Map.init(self.arena),
            .varargs = .{ .is_var_arg = true }, // main func is always vararg
            .prev = null,
        };

        self.func = main_func;

        for (chunk.body) |node| {
            try self.genNode(node);
        }
        std.debug.assert(self.func.max_stack_size >= self.func.free_register);
        std.debug.assert(self.func.free_register >= self.func.nactvar);

        self.func.free_register = self.func.nactvar;

        // In the PUC Lua implementation, this final return is added in close_func.
        // It is added regardless of whether or not there is already a return, e.g.
        // a file with just `return 1` in it will actually have 2 return instructions
        // (one for the explicit return and then this one)
        _ = try self.func.emitReturn(0, 0);

        return main_func;
    }

    pub fn genNode(self: *Compiler, node: *Node) Error!void {
        switch (node.id) {
            .chunk => unreachable, // should call genChunk directly, it should always be the root of a tree
            .call => try self.genCall(@fieldParentPtr(Node.Call, "base", node)),
            .literal => try self.genLiteral(@fieldParentPtr(Node.Literal, "base", node)),
            .identifier => try self.genIdentifier(@fieldParentPtr(Node.Identifier, "base", node)),
            .return_statement => try self.genReturnStatement(@fieldParentPtr(Node.ReturnStatement, "base", node)),
            else => unreachable, // TODO
        }
    }

    // TODO this probably might not cut it for something like
    // return 1 + 2
    pub fn genReturnStatement(self: *Compiler, return_statement: *Node.ReturnStatement) Error!void {
        var first_return_reg: u8 = 0;
        var num_return_values: u9 = @intCast(u9, return_statement.values.len);

        for (return_statement.values) |value_node| {
            // TODO hasmultret
            try self.genNode(value_node);
            if (return_statement.values.len == 1) {
                first_return_reg = try self.func.exp2anyreg(&self.func.cur_exp);
            } else {
                try self.func.exp2nextreg(&self.func.cur_exp);
            }
        }
        if (return_statement.values.len > 1) {
            first_return_reg = self.func.nactvar;
            std.debug.assert(num_return_values == self.func.free_register - first_return_reg);
        }
        _ = try self.func.emitReturn(first_return_reg, num_return_values);
    }

    pub fn genCall(self: *Compiler, call: *Node.Call) Error!void {
        try self.genNode(call.expression);
        try self.func.exp2nextreg(&self.func.cur_exp);
        const func_exp = self.func.cur_exp;
        std.debug.assert(func_exp.kind == .nonreloc);
        const base: u8 = @intCast(u8, func_exp.value.?.reg);

        for (call.arguments) |argument_node| {
            try self.genNode(argument_node);
            try self.func.exp2nextreg(&self.func.cur_exp);
        }
        var nparams: usize = 0;
        if (call.arguments.len > 0) {
            nparams = self.func.free_register - (base + 1);
        }

        try self.func.code.append(
            @bitCast(Instruction, InstructionABC.init(
                .call,
                @intCast(u8, base),
                @intCast(u9, nparams + 1),
                1, // 1 = no assignment, 2 = assignment TODO assignment
            )),
        );
        // call removes function and arguments, and leaves (unless changed) one result
        self.func.free_register = base + 1;
    }

    pub fn genLiteral(self: *Compiler, literal: *Node.Literal) Error!void {
        switch (literal.token.id) {
            .string => {
                const string_source = self.source[literal.token.start..literal.token.end];
                var buf = try self.arena.alloc(u8, string_source.len);
                defer self.arena.free(buf);
                const parsed = zua.parse_literal.parseString(string_source, buf);
                const index = try self.putConstant(Constant{ .string = parsed });
                self.func.cur_exp = .{
                    .kind = .index_of_constant,
                    .value = .{ .index = index },
                };
            },
            .number => {
                const number_source = self.source[literal.token.start..literal.token.end];
                const parsed = zua.parse_literal.parseNumber(number_source);
                self.func.cur_exp = .{
                    .kind = .number,
                    .value = .{ .number = parsed },
                };
            },
            .keyword_true => {
                self.func.cur_exp = .{
                    .kind = .@"true",
                };
            },
            .keyword_false => {
                self.func.cur_exp = .{
                    .kind = .@"false",
                };
            },
            .keyword_nil => {
                self.func.cur_exp = .{
                    .kind = .nil,
                };
            },
            else => unreachable,
        }
    }

    pub fn genIdentifier(self: *Compiler, node: *Node.Identifier) Error!void {
        const name = self.source[node.token.start..node.token.end];
        const index = try self.putConstant(Constant{ .string = name });
        // TODO distinguish between global and local vars
        self.func.cur_exp = .{
            .kind = .global,
            .value = .{ .index = index },
        };
    }

    pub fn putConstant(self: *Compiler, constant: Constant) Error!usize {
        var final_constant = constant;
        if (constant == .string and !self.func.constants_map.contains(constant)) {
            // dupe the string so that the resulting Function owns all the memory
            // TODO how should this memory get cleaned up on compile error?
            const dupe = try self.allocator.dupe(u8, constant.string);
            final_constant = Constant{ .string = dupe };
        }
        return self.func.putConstant(final_constant);
    }
};

fn getLuacDump(allocator: *Allocator, source: []const u8) ![]const u8 {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const child = try std.ChildProcess.init(&[_][]const u8{
        "luac",
        "-s",
        "-",
    }, allocator);
    defer child.deinit();

    child.stdin_behavior = .Pipe;
    child.stdout_behavior = .Ignore;
    child.stderr_behavior = .Ignore;
    child.cwd = &tmp.sub_path;
    child.cwd_dir = tmp.dir;

    try child.spawn();

    try child.stdin.?.writer().writeAll(source);

    // close stdin and mark it as closed
    // TODO is there a more intended way to signal stdin EOF?
    child.stdin.?.close();
    child.stdin = null;

    const term = try child.wait();
    switch (term) {
        .Exited => |code| {
            if (code != 0) {
                return error.ExitCodeFailure;
            }
        },
        .Signal, .Stopped, .Unknown => |code| {
            return error.ProcessTerminated;
        },
    }

    return tmp.dir.readFileAlloc(allocator, "luac.out", std.math.maxInt(usize));
}

fn testCompile(source: []const u8) !void {
    var chunk = try compile(std.testing.allocator, source);
    defer chunk.deinit();

    try zua.debug.checkcode(&chunk);

    var buf = std.ArrayList(u8).init(std.testing.allocator);
    defer buf.deinit();
    try zua.dump.write(chunk, buf.writer());

    const luacDump = try getLuacDump(std.testing.allocator, source);
    defer std.testing.allocator.free(luacDump);

    std.testing.expectEqualSlices(u8, luacDump, buf.items);
}

test "compile hello world" {
    try testCompile("print \"hello world\"");
}

test "compile print multiple literals" {
    try testCompile("print(nil, true)");
    try testCompile("print(nil, true, false, 1)");
}

test "compile return statements" {
    try testCompile("return");
    try testCompile("return false");
    try testCompile("return false, true, \"hello\"");
}
