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
const Token = zua.lex.Token;

/// LUAI_MAXVARS from lconf.h
pub const max_vars = 200;

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
        num_active_local_vars: u8 = 0,
        active_local_vars: [max_vars]usize = undefined,
        local_vars: std.ArrayList(LocalVar),

        pub const LocalVar = struct {
            name_token: Token,
            active_instruction_index: usize,
            dead_instruction_index: usize,
        };

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

        pub fn exp2nextreg(self: *Func, e: *ExpDesc) !u8 {
            try self.dischargevars(e);
            try self.freeexp(e);
            try self.reserveregs(1);
            const reg = self.free_register - 1;
            try self.exp2reg(e, reg);
            return reg;
        }

        pub fn exp2anyreg(self: *Func, e: *ExpDesc) !u8 {
            try self.dischargevars(e);
            if (e.desc == .nonreloc) {
                const reg = e.desc.nonreloc.result_register;
                // exp is already in a register
                if (!e.hasjumps()) return reg;
                // reg is not a local?
                if (reg >= self.num_active_local_vars) {
                    try self.exp2reg(e, reg);
                    return reg;
                }
            }
            return try self.exp2nextreg(e);
        }

        pub fn dischargevars(self: *Func, e: *ExpDesc) !void {
            switch (e.desc) {
                .local_register => {
                    const reg = e.desc.local_register;
                    e.desc = .{ .nonreloc = .{ .result_register = reg } };
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
                    self.freereg(e.desc.indexed.key_register_or_constant_index);
                    self.freereg(e.desc.indexed.table_register);
                    const instruction_index = try self.emitABC(.gettable, 0, e.desc.indexed.table_register, e.desc.indexed.key_register_or_constant_index);
                    e.desc = .{ .relocable = .{ .instruction_index = instruction_index } };
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
                const instruction_index = e.desc.vararg.instruction_index;
                e.desc = .{ .relocable = .{ .instruction_index = instruction_index } };
            }
        }

        pub fn freeexp(self: *Func, e: *ExpDesc) !void {
            if (e.desc == .nonreloc) {
                self.freereg(e.desc.nonreloc.result_register);
            }
        }

        pub fn freereg(self: *Func, reg: u9) void {
            if (!zua.opcodes.isConstant(reg) and reg >= self.num_active_local_vars) {
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
                    _ = try self.emitNil(reg, 1);
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
                        const result_register = e.desc.nonreloc.result_register;
                        _ = try self.emitABC(.move, reg, result_register, 0);
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

        // TODO better param names
        pub fn emitNil(self: *Func, from: u8, n: usize) !?usize {
            // TODO other branches
            if (true) { // TODO fs->pc > fs->lasttarget
                if (self.pc() == 0) { // function start?
                    if (from >= self.num_active_local_vars) {
                        return null; // positions are already clean
                    }
                }
            }
            const b = @intCast(u9, from + n - 1);
            if (self.emitInstruction(Instruction.ABC.init(.loadnil, from, b, 0))) |index| {
                return index;
            } else |err| {
                return err;
            }
        }

        /// Appends a new instruction to the Func's code and returns the
        /// index of the added instruction
        pub fn emitInstruction(self: *Func, instruction: anytype) !usize {
            try self.code.append(@bitCast(Instruction, instruction));
            return self.pc() - 1;
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

        pub fn new_localvar(self: *Func, name_token: Token, var_index: usize) Error!void {
            const active_local_var_index = self.num_active_local_vars + var_index;
            if (active_local_var_index >= max_vars) {
                @panic("TODO too many local vars error");
            }
            self.active_local_vars[active_local_var_index] = try self.registerlocalvar(name_token);
        }

        pub fn registerlocalvar(self: *Func, name_token: Token) Error!usize {
            try self.local_vars.append(.{
                .name_token = name_token,
                // to be filled in later
                .active_instruction_index = undefined,
                .dead_instruction_index = undefined,
            });
            return self.local_vars.items.len - 1;
        }

        pub fn adjust_assign(self: *Func, num_vars: usize, num_values: usize, e: *ExpDesc) !void {
            var extra: isize = @intCast(isize, num_vars) - @intCast(isize, num_values);
            if (e.hasmultret()) {
                extra += 1;
                if (extra < 0) extra = 0;
                try self.setreturns(e, @intCast(u9, extra));
                if (extra > 1) {
                    try self.reserveregs(@intCast(u8, extra - 1));
                }
            } else {
                if (e.desc != .@"void") {
                    _ = try self.exp2nextreg(e);
                }
                if (extra > 0) {
                    const reg = self.free_register;
                    try self.reserveregs(@intCast(u8, extra));
                    _ = try self.emitNil(reg, @intCast(usize, extra));
                }
            }
        }

        pub fn setreturns(self: *Func, e: *ExpDesc, num_results: u9) !void {
            if (e.desc == .call) {
                const instruction = @ptrCast(*Instruction.Call, self.getcode(e));
                instruction.setNumReturnValues(num_results);
            } else if (e.desc == .vararg) {
                const instruction = @ptrCast(*Instruction.VarArg, self.getcode(e));
                instruction.setNumReturnValues(num_results);
                instruction.setFirstReturnValueRegister(self.free_register);
                try self.reserveregs(1);
            }
        }

        pub fn adjustlocalvars(self: *Func, num_vars: usize) !void {
            self.num_active_local_vars += @intCast(u8, num_vars);
            var num_vars_remaining = num_vars;
            while (num_vars_remaining > 0) : (num_vars_remaining -= 1) {
                const local_var = self.getlocvar(self.num_active_local_vars - num_vars_remaining);
                local_var.active_instruction_index = self.pc();
            }
        }

        pub fn removevars(self: *Func, to_level: u8) !void {
            while (self.num_active_local_vars > to_level) {
                self.num_active_local_vars -= 1;
                const local_var = self.getlocvar(self.num_active_local_vars);
                local_var.dead_instruction_index = self.pc();
            }
        }

        pub fn getlocvar(self: *Func, active_local_var_index: usize) *LocalVar {
            const local_var_index = self.active_local_vars[active_local_var_index];
            return &self.local_vars.items[active_local_var_index];
        }

        /// searchvar equivalent
        /// Returns the index to the active local var, if found
        pub fn findLocalVarByToken(self: *Func, name_token: Token, source: []const u8) ?usize {
            if (self.num_active_local_vars == 0) return null;

            const name_to_find = source[name_token.start..name_token.end];
            var i: usize = self.num_active_local_vars - 1;
            while (true) : (i -= 1) {
                const cur_name_token = self.getlocvar(i).name_token;
                const cur_name = source[cur_name_token.start..cur_name_token.end];
                if (std.mem.eql(u8, cur_name, name_to_find)) {
                    return i;
                }
                if (i == 0) break;
            }
            return null;
        }

        pub fn exp2val(self: *Func, e: *ExpDesc) !void {
            if (e.hasjumps()) {
                _ = try self.exp2anyreg(e);
            } else {
                try self.dischargevars(e);
            }
        }

        pub fn exp2RK(self: *Func, e: *ExpDesc) !u9 {
            try self.exp2val(e);
            switch (e.desc) {
                .number, .@"true", .@"false", .nil => {
                    if (self.constants.items.len <= zua.opcodes.max_constant_index) {
                        var constant: Constant = switch (e.desc) {
                            .nil => Constant{ .nil = {} },
                            .@"true", .@"false" => Constant{ .boolean = e.desc == .@"true" },
                            .number => Constant{ .number = e.desc.number },
                            else => unreachable,
                        };
                        const index = try self.putConstant(constant);
                        return zua.opcodes.constantIndexToRK(@intCast(u9, index));
                    }
                },
                .constant_index => {
                    if (e.desc.constant_index <= zua.opcodes.max_constant_index) {
                        return zua.opcodes.constantIndexToRK(@intCast(u9, e.desc.constant_index));
                    }
                },
                else => {},
            }
            // not a constant in the right range, put it in a register
            return @intCast(u9, try self.exp2anyreg(e));
        }

        pub fn indexed(self: *Func, table: *ExpDesc, key: *ExpDesc) !void {
            const key_register_or_constant_index = try self.exp2RK(key);
            // TODO can this be some other type here?
            const table_register = table.desc.nonreloc.result_register;
            table.desc = .{
                .indexed = .{
                    .table_register = table_register,
                    .key_register_or_constant_index = key_register_or_constant_index,
                },
            };
        }

        /// luaK_self equivalent
        pub fn handleSelf(self: *Func, e: *ExpDesc, key: *ExpDesc) !void {
            _ = try self.exp2anyreg(e);
            try self.freeexp(e);
            const func_reg = self.free_register;
            try self.reserveregs(2);
            // TODO can this be a different type?
            const result_register = e.desc.nonreloc.result_register;
            const key_rk = try self.exp2RK(key);
            _ = try self.emitABC(.self, func_reg, result_register, key_rk);
            try self.freeexp(key);
            e.desc = .{ .nonreloc = .{ .result_register = func_reg } };
        }

        pub fn storevar(self: *Func, var_e: *ExpDesc, e: *ExpDesc) !void {
            switch (var_e.desc) {
                .local_register => {
                    @panic("TODO");
                },
                .upvalue_index => {
                    @panic("TODO");
                },
                .global => {
                    const reg = try self.exp2anyreg(e);
                    const name_constant_index = var_e.desc.global.name_constant_index;
                    _ = try self.emitABx(.setglobal, reg, @intCast(u18, name_constant_index));
                },
                .indexed => {
                    @panic("TODO");
                },
                else => unreachable,
            }
            try self.freeexp(e);
        }

        pub fn setlist(self: *Func, base: u8, num_values: usize, to_store: ?usize) !void {
            const flush_batch_num: usize = (num_values - 1) / Instruction.SetList.fields_per_flush + 1;
            const num_values_in_batch: u9 = if (to_store == null) 0 else @intCast(u9, to_store.?);
            if (flush_batch_num <= Instruction.ABC.max_c) {
                _ = try self.emitABC(.setlist, base, num_values_in_batch, @intCast(u9, flush_batch_num));
            } else {
                _ = try self.emitABC(.setlist, base, num_values_in_batch, 0);
                @panic("TODO oversized flush batch num");
            }
            self.free_register = base + 1;
        }

        /// Current instruction pointer
        pub fn pc(self: *Func) usize {
            return self.code.items.len;
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

        pub fn hasmultret(self: *ExpDesc) bool {
            return self.desc == .call or self.desc == .vararg;
        }
    };

    pub fn genChunk(self: *Compiler, chunk: *Node.Chunk) Error!*Func {
        var main_func: *Func = try self.arena.create(Func);
        main_func.* = .{
            .code = std.ArrayList(Instruction).init(self.arena),
            .constants = std.ArrayList(Constant).init(self.arena),
            .constants_map = Constant.Map.init(self.arena),
            .local_vars = std.ArrayList(Func.LocalVar).init(self.arena),
            .varargs = .{ .is_var_arg = true }, // main func is always vararg
            .prev = null,
        };

        self.func = main_func;

        for (chunk.body) |node| {
            try self.genNode(node);

            std.debug.assert(self.func.max_stack_size >= self.func.free_register);
            std.debug.assert(self.func.free_register >= self.func.num_active_local_vars);

            self.func.free_register = self.func.num_active_local_vars;
        }

        try self.func.removevars(0);

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
            .assignment_statement => try self.genAssignmentStatement(@fieldParentPtr(Node.AssignmentStatement, "base", node)),
            .literal => try self.genLiteral(@fieldParentPtr(Node.Literal, "base", node)),
            .identifier => try self.genIdentifier(@fieldParentPtr(Node.Identifier, "base", node)),
            .return_statement => try self.genReturnStatement(@fieldParentPtr(Node.ReturnStatement, "base", node)),
            .field_access => try self.genFieldAccess(@fieldParentPtr(Node.FieldAccess, "base", node)),
            .index_access => try self.genIndexAccess(@fieldParentPtr(Node.IndexAccess, "base", node)),
            .table_constructor => try self.genTableConstructor(@fieldParentPtr(Node.TableConstructor, "base", node)),
            .table_field => unreachable, // should never be called outside of genTableConstructor
            else => unreachable, // TODO
        }
    }

    pub fn genTableConstructor(self: *Compiler, table_constructor: *Node.TableConstructor) Error!void {
        const instruction_index = try self.func.emitABC(.newtable, 0, 0, 0);
        self.func.cur_exp = .{ .desc = .{ .relocable = .{ .instruction_index = instruction_index } } };
        const table_reg = try self.func.exp2nextreg(&self.func.cur_exp);

        var num_array_values: u9 = 0;
        var unflushed_array_values: u9 = 0;
        for (table_constructor.fields) |field_node_base| {
            const prev_exp = self.func.cur_exp;

            const field_node = @fieldParentPtr(Node.TableField, "base", field_node_base);
            try self.genTableField(field_node);
            if (field_node.key == null) {
                num_array_values += 1;
                unflushed_array_values += 1;
            }

            if (unflushed_array_values >= Instruction.SetList.fields_per_flush) {
                try self.func.setlist(table_reg, num_array_values, unflushed_array_values);
                unflushed_array_values = 0;
            }

            self.func.cur_exp = prev_exp;
        }

        if (unflushed_array_values > 0) {
            // TODO MULTRET
            try self.func.setlist(table_reg, num_array_values, unflushed_array_values);
        }

        if (table_constructor.fields.len > 0) {
            const newtable_instruction = @ptrCast(*Instruction.NewTable, &self.func.code.items[instruction_index]);
            newtable_instruction.setArraySize(num_array_values);
            newtable_instruction.setTableSize(@intCast(u9, table_constructor.fields.len) - num_array_values);
        }
    }

    pub fn genTableField(self: *Compiler, table_field: *Node.TableField) Error!void {
        if (table_field.key == null) {
            try self.genNode(table_field.value);
            _ = try self.func.exp2nextreg(&self.func.cur_exp);
        } else {
            const table_reg = self.func.cur_exp.desc.nonreloc.result_register;
            const prev_free_reg = self.func.free_register;

            try self.genNode(table_field.key.?);
            const key_rk = try self.func.exp2RK(&self.func.cur_exp);

            try self.genNode(table_field.value);
            const val_rk = try self.func.exp2RK(&self.func.cur_exp);

            _ = try self.func.emitInstruction(Instruction.SetTable.init(table_reg, key_rk, val_rk));

            self.func.free_register = prev_free_reg;
        }
    }

    pub fn genAssignmentStatement(self: *Compiler, assignment_statement: *Node.AssignmentStatement) Error!void {
        if (assignment_statement.is_local) {
            for (assignment_statement.variables) |variable_node, i| {
                // we can be certain that this is an identifier when assigning with the local keyword
                const identifier_node = @fieldParentPtr(Node.Identifier, "base", variable_node);
                const name_token = identifier_node.token;
                try self.func.new_localvar(name_token, i);
            }
            try self.genExpList1(assignment_statement.values);

            if (assignment_statement.values.len == 0) {
                self.func.cur_exp = .{
                    .desc = .{ .@"void" = {} },
                };
            }
            try self.func.adjust_assign(assignment_statement.variables.len, assignment_statement.values.len, &self.func.cur_exp);
            try self.func.adjustlocalvars(assignment_statement.variables.len);
        } else {
            // TODO check_conflict
            // TODO checklimit 'variables in assignment'
            const var_exps = try self.arena.alloc(ExpDesc, assignment_statement.variables.len);
            defer self.arena.free(var_exps);

            for (assignment_statement.variables) |variable_node, i| {
                try self.genNode(variable_node);
                // store the ExpDesc's for use later
                var_exps[i] = self.func.cur_exp;
            }
            try self.genExpList1(assignment_statement.values);

            var last_taken_care_of = false;
            if (assignment_statement.values.len != assignment_statement.variables.len) {
                try self.func.adjust_assign(assignment_statement.variables.len, assignment_statement.values.len, &self.func.cur_exp);
                if (assignment_statement.values.len > assignment_statement.variables.len) {
                    // remove extra values
                    self.func.free_register -= @intCast(u8, assignment_statement.values.len - assignment_statement.variables.len);
                }
            } else {
                try self.func.setoneret(&self.func.cur_exp);
                try self.func.storevar(&var_exps[var_exps.len - 1], &self.func.cur_exp);
                last_taken_care_of = true;
            }

            // traverse in reverse order to maintain compatibility with
            // PUC Lua bytecode order
            var unstored_index: usize = assignment_statement.variables.len - 1;
            if (last_taken_care_of and unstored_index > 0) unstored_index -= 1;
            const finished: bool = unstored_index == 0 and last_taken_care_of;
            while (!finished) : (unstored_index -= 1) {
                self.func.cur_exp = .{ .desc = .{ .nonreloc = .{ .result_register = self.func.free_register - 1 } } };
                try self.func.storevar(&var_exps[unstored_index], &self.func.cur_exp);
                if (unstored_index == 0) break;
            }
        }
    }

    /// helper function equivalent to explist1 in lparser.c
    fn genExpList1(self: *Compiler, nodes: []*Node) Error!void {
        for (nodes) |node, i| {
            try self.genNode(node);
            // skip the last one
            if (i != nodes.len - 1) {
                _ = try self.func.exp2nextreg(&self.func.cur_exp);
            }
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
                _ = try self.func.exp2nextreg(&self.func.cur_exp);
            }
        }
        if (return_statement.values.len > 1) {
            first_return_reg = self.func.num_active_local_vars;
            std.debug.assert(num_return_values == self.func.free_register - first_return_reg);
        }
        _ = try self.func.emitReturn(first_return_reg, num_return_values);
    }

    pub fn genCall(self: *Compiler, call: *Node.Call) Error!void {
        try self.genNode(call.expression);
        var is_self_call = false;
        if (call.expression.id == .field_access) {
            const field_access_node = @fieldParentPtr(Node.FieldAccess, "base", call.expression);
            is_self_call = field_access_node.separator.isChar(':');
        }
        if (!is_self_call) {
            _ = try self.func.exp2nextreg(&self.func.cur_exp);
        }
        const func_exp = self.func.cur_exp;
        std.debug.assert(func_exp.desc == .nonreloc);
        const base: u8 = @intCast(u8, func_exp.desc.nonreloc.result_register);

        for (call.arguments) |argument_node| {
            try self.genNode(argument_node);
            _ = try self.func.exp2nextreg(&self.func.cur_exp);
        }
        var nparams = self.func.free_register - (base + 1);

        // assume 1 return value if this is not a statement, will be modified as necessary later
        const num_return_values: u9 = if (call.is_statement) 0 else 1;
        const index = try self.func.emitInstruction(Instruction.Call.init(base, @intCast(u9, nparams), num_return_values));
        self.func.cur_exp = .{ .desc = .{ .call = .{ .instruction_index = index } } };

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
            .ellipsis => {
                const instruction_index = try self.func.emitInstruction(Instruction.VarArg.init(0, 0));
                self.func.cur_exp = .{ .desc = .{ .vararg = .{ .instruction_index = instruction_index } } };
            },
            .name => {
                const name = self.source[literal.token.start..literal.token.end];
                const constant_index = try self.putConstant(Constant{ .string = name });
                self.func.cur_exp = .{ .desc = .{ .constant_index = constant_index } };
            },
            else => unreachable,
        }
    }

    pub fn genIdentifier(self: *Compiler, node: *Node.Identifier) Error!void {
        if (self.func.findLocalVarByToken(node.token, self.source)) |active_local_var_index| {
            self.func.cur_exp = .{ .desc = .{ .local_register = @intCast(u8, active_local_var_index) } };
            // TODO if (!base) markupval()
        } else {
            // TODO upvalues
            const name = self.source[node.token.start..node.token.end];
            const index = try self.putConstant(Constant{ .string = name });
            self.func.cur_exp = .{ .desc = .{ .global = .{ .name_constant_index = index } } };
        }
    }

    pub fn genFieldAccess(self: *Compiler, node: *Node.FieldAccess) Error!void {
        if (node.separator.isChar(':')) {
            try self.genNode(node.prefix);

            const name = self.source[node.field.start..node.field.end];
            const constant_index = try self.putConstant(Constant{ .string = name });
            var key = ExpDesc{ .desc = .{ .constant_index = constant_index } };

            try self.func.handleSelf(&self.func.cur_exp, &key);
        } else {
            try self.genNode(node.prefix);
            _ = try self.func.exp2anyreg(&self.func.cur_exp);

            const name = self.source[node.field.start..node.field.end];
            const constant_index = try self.putConstant(Constant{ .string = name });
            var key = ExpDesc{ .desc = .{ .constant_index = constant_index } };
            try self.func.indexed(&self.func.cur_exp, &key);
        }
    }

    pub fn genIndexAccess(self: *Compiler, node: *Node.IndexAccess) Error!void {
        try self.genNode(node.prefix);
        _ = try self.func.exp2anyreg(&self.func.cur_exp);
        var table_exp = self.func.cur_exp;

        // reset and then restore afterwards
        self.func.cur_exp = ExpDesc{ .desc = .{ .@"void" = {} } };

        try self.genNode(node.index);
        try self.func.exp2val(&self.func.cur_exp);
        try self.func.indexed(&table_exp, &self.func.cur_exp);

        self.func.cur_exp = table_exp;
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

    //std.debug.print("\n", .{});
    //chunk.printCode();

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

test "compile local statements" {
    try testCompile("local a = 1");
    try testCompile(
        \\local a = "hello world"
        \\print(a)
    );
    try testCompile("local a, b");
    try testCompile("local a, b = 1");
    try testCompile("local a, b = 1, 2, 3");
}

test "assignment from function return values" {
    try testCompile("local a = f()");
    try testCompile(
        \\local a = f()
        \\print(a)
    );
    try testCompile("local a, b = f()");
    try testCompile("local a, b = f(), g()");
}

test "vararg" {
    try testCompile("local a = ...");
    try testCompile("local a, b, c = ...");
    try testCompile(
        \\local a, b, c = ...
        \\print(a, b, c)
    );
}

test "gettable" {
    try testCompile("a.b()");
    try testCompile("a.b(c.a)");
    try testCompile("a[true]()");
    try testCompile("a[1]()");
}

test "self" {
    try testCompile("a:b()");
    try testCompile("a:b(1,2,3)");
}

test "setglobal" {
    try testCompile("a = 1");
    try testCompile("a, b, c = 1, 2, 3");
    try testCompile("a = 1, 2, 3");
    try testCompile("a, b, c = 1");
}

test "newtable" {
    try testCompile("return {}");
    try testCompile("return {a=1}");
    try testCompile("return {[a]=1}");
    try testCompile("return {a=1, b=2, c=3}");
    try testCompile("return {1}");
    try testCompile("return {1,2,3}");
    try testCompile("return {1, a=2, 3}");
    try testCompile("return {1, 2, a=b, 3}");
    try testCompile("return {a=f()}");
    try testCompile("return {" ++ ("a," ** 51) ++ "}");

    // TODO known broken cases
    //try testCompile("return {...}");
    //try testCompile("return {f()}");
}
