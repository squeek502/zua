const std = @import("std");
const Allocator = std.mem.Allocator;
const zua = @import("zua.zig");
const Instruction = zua.opcodes.Instruction;
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
        cur_exp: ExpDesc = .{ .desc = .{ .@"void" = {} } },
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
            if (e.desc == .nonreloc) {
                const reg = e.desc.nonreloc.result_register;
                // exp is already in a register
                if (!e.hasjumps()) return reg;
                // reg is not a local?
                if (reg >= self.nactvar) {
                    try self.exp2reg(e, reg);
                    return reg;
                }
            }

            try self.exp2nextreg(e);
            return e.desc.nonreloc.result_register;
        }

        pub fn dischargevars(self: *Func, e: *ExpDesc) !void {
            switch (e.desc) {
                .local_register => {
                    e.desc = .{ .nonreloc = .{ .result_register = e.desc.local_register } };
                },
                .upvalue_index => {
                    //const index = try self.emitABC(.getupval, 0, @intCast(u18, e.val.?), 0);
                    //e.val = @intCast(isize, index);
                    e.desc = .{ .relocable = .{ .instruction_index = 0 } };
                    @panic("TODO");
                },
                .global => {
                    const index = try self.emitABx(.getglobal, 0, @intCast(u18, e.desc.global.name_constant_index));
                    e.desc = .{ .relocable = .{ .instruction_index = index } };
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
            if (e.desc == .call) {
                const instruction = self.getcode(e);
                e.desc = .{ .nonreloc = .{ .result_register = instruction.a } };
            } else if (e.desc == .vararg) {
                const instruction = self.getcode(e);
                const instructionABC = @ptrCast(*Instruction.ABC, instruction);
                instructionABC.*.b = 2;
                e.desc = .{ .relocable = .{ .instruction_index = e.desc.vararg.instruction_index } };
            }
        }

        pub fn freeexp(self: *Func, e: *ExpDesc) !void {
            if (e.desc == .nonreloc) {
                self.freereg(e.desc.nonreloc.result_register);
            }
        }

        pub fn freereg(self: *Func, reg: u8) void {
            if (!zua.opcodes.isConstant(reg) and reg >= self.nactvar) {
                self.free_register -= 1;
                std.debug.assert(reg == self.free_register);
            }
        }

        pub fn getcode(self: *Func, e: *ExpDesc) *Instruction {
            const index: usize = switch (e.desc) {
                .jmp, .relocable, .call, .vararg => |desc| desc.instruction_index,
                else => unreachable,
            };
            return &self.code.items[index];
        }

        pub fn exp2reg(self: *Func, e: *ExpDesc, reg: u8) !void {
            try self.discharge2reg(e, reg);
            if (e.desc == .jmp) {
                //self.concat(...)
                @panic("TODO");
            }
            if (e.hasjumps()) {
                @panic("TODO");
            }
            e.* = .{
                .desc = .{
                    .nonreloc = .{ .result_register = reg },
                },
                .patch_list = null,
            };
        }

        pub fn discharge2reg(self: *Func, e: *ExpDesc, reg: u8) !void {
            try self.dischargevars(e);
            switch (e.desc) {
                .nil => {
                    _ = try self.emitABC(.loadnil, reg, 1, 0);
                },
                .@"false", .@"true" => {
                    _ = try self.emitInstruction(Instruction.LoadBool.init(reg, e.desc == .@"true", false));
                },
                .constant_index => {
                    _ = try self.emitABx(.loadk, reg, @intCast(u18, e.desc.constant_index));
                },
                .number => {
                    // Need to add number constants to the constant table here instead of,
                    // in genLiteral because not every constant is needed in the final
                    // bytecode, i.e. `return 1 + 2` should be resolved to only need the
                    // constant `3` (1 and 2 are not in the final bytecode)
                    const index = try self.putConstant(Constant{ .number = e.desc.number });
                    _ = try self.emitABx(.loadk, reg, @intCast(u18, index));
                },
                .relocable => {
                    const instruction = self.getcode(e);
                    const instructionABC = @ptrCast(*Instruction.ABC, instruction);
                    instructionABC.*.a = reg;
                },
                .nonreloc => {
                    if (reg != e.desc.nonreloc.result_register) {
                        //_ = try self.emitABC(.move, reg, e.val.?, 0);
                    }
                },
                .@"void", .jmp => return, // nothing to do
                else => unreachable,
            }
            e.desc = .{ .nonreloc = .{ .result_register = reg } };
        }

        /// luaK_ret equivalent
        pub fn emitReturn(self: *Func, first_return_reg: u8, num_returns: u9) !usize {
            return self.emitInstruction(Instruction.Return.init(first_return_reg, num_returns));
        }

        /// Appends a new instruction to the Func's code and returns the
        /// index of the added instruction
        pub fn emitInstruction(self: *Func, instruction: anytype) !usize {
            try self.code.append(@bitCast(Instruction, instruction));
            return self.code.items.len - 1;
        }

        /// Appends a new instruction to the Func's code and returns the
        /// index of the added instruction
        /// luaK_codeABC equivalent
        pub fn emitABC(self: *Func, op: OpCode, a: u8, b: u9, c: u9) !usize {
            return self.emitInstruction(Instruction.ABC.init(op, a, b, c));
        }

        /// Appends a new instruction to the Func's code and returns the
        /// index of the added instruction
        /// luaK_codeABx equivalent
        pub fn emitABx(self: *Func, op: OpCode, a: u8, bx: u18) !usize {
            return self.emitInstruction(Instruction.ABx.init(op, a, bx));
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
        desc: union(ExpDesc.Kind) {
            @"void": void,
            nil: void,
            @"true": void,
            @"false": void,
            constant_index: usize,
            number: f64,
            local_register: u8,
            upvalue_index: usize,
            global: struct {
                name_constant_index: usize,
            },
            indexed: struct {
                table_register: u8,
                key_register_or_constant_index: u9,
            },
            jmp: InstructionIndex,
            relocable: InstructionIndex,
            nonreloc: struct {
                result_register: u8,
            },
            call: InstructionIndex,
            vararg: InstructionIndex,
        },
        // TODO the types here should be revisited
        patch_list: ?struct {
            exit_when_true: ?i32,
            exit_when_false: ?i32,
        } = null,

        // A wrapper struct for usize so that it can have a descriptive name
        // and each tag that uses it can share the same type
        pub const InstructionIndex = struct {
            instruction_index: usize,
        };

        pub const Kind = enum {
            @"void",
            nil,
            @"true",
            @"false",
            constant_index,
            number,
            local_register,
            upvalue_index,
            global,
            indexed,
            jmp,
            relocable,
            nonreloc,
            call,
            vararg,
        };

        pub fn hasjumps(self: *ExpDesc) bool {
            return self.patch_list != null;
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
        std.debug.assert(func_exp.desc == .nonreloc);
        const base: u8 = @intCast(u8, func_exp.desc.nonreloc.result_register);

        for (call.arguments) |argument_node| {
            try self.genNode(argument_node);
            try self.func.exp2nextreg(&self.func.cur_exp);
        }
        var nparams: usize = 0;
        if (call.arguments.len > 0) {
            nparams = self.func.free_register - (base + 1);
        }

        // TODO assignment
        _ = try self.func.emitInstruction(Instruction.Call.init(base, @intCast(u9, nparams), 0));

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
                self.func.cur_exp.desc = .{ .constant_index = index };
            },
            .number => {
                const number_source = self.source[literal.token.start..literal.token.end];
                const parsed = zua.parse_literal.parseNumber(number_source);
                self.func.cur_exp.desc = .{ .number = parsed };
            },
            .keyword_true => {
                self.func.cur_exp.desc = .{ .@"true" = .{} };
            },
            .keyword_false => {
                self.func.cur_exp.desc = .{ .@"false" = .{} };
            },
            .keyword_nil => {
                self.func.cur_exp.desc = .{ .nil = {} };
            },
            else => unreachable,
        }
    }

    pub fn genIdentifier(self: *Compiler, node: *Node.Identifier) Error!void {
        const name = self.source[node.token.start..node.token.end];
        const index = try self.putConstant(Constant{ .string = name });
        // TODO distinguish between global and local vars
        self.func.cur_exp.desc = .{ .global = .{ .name_constant_index = index } };
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
