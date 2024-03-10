const std = @import("std");
const zua = @import("zua.zig");
const Function = zua.object.Function;
const OpCode = zua.opcodes.OpCode;
const Instruction = zua.opcodes.Instruction;
const max_stack_size = zua.parse.max_stack_size;

pub fn checkcode(function: *const Function) !void {
    _ = try symbexec(function, null);
}

fn precheck(function: *const Function) !void {
    if (function.max_stack_size > max_stack_size) return error.MaxStackSizeTooBig;
    if (function.num_params > function.max_stack_size) return error.StackTooSmallForParams;
    if (!function.varargs.needs_arg and function.varargs.has_arg) return error.HasUnneededArg;
    // check(pt->sizeupvalues <= pt->nups);
    // check(pt->sizelineinfo == pt->sizecode || pt->sizelineinfo == 0);
    // check(pt->sizecode > 0 && GET_OPCODE(pt->code[pt->sizecode-1]) == OP_RETURN);
}

fn checkreg(function: *const Function, reg: usize) !void {
    if (reg >= function.max_stack_size) return error.MaxStackSizeTooSmall;
}

fn checkArgMode(function: *const Function, val: i32, mode: OpCode.OpArgMask) !void {
    switch (mode) {
        .NotUsed => {
            if (val != 0) return error.NonZeroUnusedArg;
        },
        .Used => {},
        .RegisterOrJumpOffset => {
            try checkreg(function, @intCast(val));
        },
        .ConstantOrRegisterConstant => {
            const actual_size_val: u9 = @intCast(val);
            const is_constant = zua.opcodes.rkIsConstant(actual_size_val);
            if (is_constant) {
                if (zua.opcodes.rkGetConstantIndex(actual_size_val) >= function.constants.len) return error.ConstantIndexOutOfRange;
            } else {
                try checkreg(function, actual_size_val);
            }
        },
    }
}

fn checkopenop_next(function: *const Function, index: usize) !void {
    return checkopenop(function.code[index + 1]);
}

/// luaG_checkopenop equivalent
pub fn checkopenop(instruction: Instruction) !void {
    switch (instruction.op) {
        .call,
        //.tailcall,
        .@"return",
        .setlist,
        => {
            const b = @as(Instruction.ABC, @bitCast(instruction)).b;
            if (b != 0) return error.InvalidInstructionAfterOpenCall;
        },
        else => return error.InvalidInstructionAfterOpenCall,
    }
}

fn symbexec(function: *const Function, reg: ?usize) !Instruction {
    try precheck(function);
    // defaults to final return (a 'neutral' instruction)
    var last_instruction_that_changed_reg: usize = function.code.len - 1;
    var instructions_to_skip: usize = 0;
    for (function.code, 0..) |instruction, i| {
        if (instructions_to_skip > 0) {
            instructions_to_skip -= 1;
            continue;
        }
        const a = instruction.a;
        var b: i32 = 0;
        var c: u9 = 0;
        try checkreg(function, a);
        switch (instruction.op.getOpMode()) {
            .iABC => {
                const instructionABC: Instruction.ABC = @bitCast(instruction);
                b = instructionABC.b;
                c = instructionABC.c;
                try checkArgMode(function, b, instruction.op.getBMode());
                try checkArgMode(function, c, instruction.op.getCMode());
            },
            .iABx => {
                const instructionABx: Instruction.ABx = @bitCast(instruction);
                b = instructionABx.bx;
                if (instruction.op.getBMode() == .ConstantOrRegisterConstant) {
                    if (b >= function.constants.len) return error.ConstantIndexOutOfRange;
                }
            },
            .iAsBx => {
                const instructionAsBx: Instruction.AsBx = @bitCast(instruction);
                b = instructionAsBx.getSignedBx();
                if (instruction.op.getBMode() == .RegisterOrJumpOffset) {
                    @panic("TODO");
                }
            },
        }
        if (instruction.op.setsRegisterInA()) {
            if (reg != null and a == reg.?) {
                last_instruction_that_changed_reg = i;
            }
        }
        if (instruction.op.testTMode()) {
            if (i + 2 >= function.code.len) return error.ImpossibleTestInstructionPlacement;
            // TODO OP_JMP
            //if (function.code[i+1].op != .jmp) return error.ExpectedJmpAfterTest;
        }
        switch (instruction.op) {
            .loadbool => {
                const bool_inst: Instruction.LoadBool = @bitCast(instruction);
                if (bool_inst.doesJump()) {
                    if (i + 2 >= function.code.len) return error.ImpossibleLoadBoolInstructionPlacement;
                    const next_instruction = function.code[i + 1];
                    const check = next_instruction.op != .setlist or @as(Instruction.ABC, @bitCast(next_instruction)).c != 0;
                    if (!check) return error.ImpossibleInstructionAfterLoadBool;
                }
            },
            .loadnil => {
                const loadnil_inst: Instruction.LoadNil = @bitCast(instruction);
                if (reg != null and loadnil_inst.willAssignToRegister(@intCast(reg.?))) {
                    last_instruction_that_changed_reg = i;
                }
            },
            //.getupval, .setupval => {},
            .getglobal, .setglobal => {
                const constant = function.constants[@intCast(b)];
                if (constant != .string) return error.ExpectedStringForGetOrSetGlobal;
            },
            .self => {
                try checkreg(function, a + 1);
                if (reg != null and reg.? == a + 1) {
                    last_instruction_that_changed_reg = i;
                }
            },
            .concat => {
                const concat_inst: Instruction.Concat = @bitCast(instruction);
                if (concat_inst.numConcattedValues() < 2) {
                    return error.ConcatRequiresAtLeastTwoOperands;
                }
            },
            //.tforloop => {},
            //.forloop, .forprep => {},
            //.jmp => {},
            .call, .tailcall => {
                const call_inst: Instruction.Call = @bitCast(instruction);
                if (b != 0) {
                    try checkreg(function, @intCast(a + call_inst.getNumParams()));
                }
                const num_returns = call_inst.getNumReturnValues();
                if (call_inst.isMultipleReturns()) {
                    try checkopenop_next(function, i);
                } else if (num_returns.? != 0) {
                    const result_reg_end = call_inst.getResultRegEnd().?;
                    try checkreg(function, result_reg_end);
                }
                if (reg != null and reg.? >= a) {
                    last_instruction_that_changed_reg = i;
                }
            },
            .@"return" => {
                const ret_inst: Instruction.Return = @bitCast(instruction);
                const num_returns = ret_inst.getNumReturnValues();
                if (num_returns != null and num_returns.? > 0) {
                    try checkreg(function, @intCast(a + num_returns.? - 1));
                }
            },
            .setlist => {
                const setlist_inst: Instruction.SetList = @bitCast(instruction);
                if (b > 0) {
                    try checkreg(function, @intCast(a + b));
                }
                if (setlist_inst.isBatchNumberStoredInNextInstruction()) {
                    if (i + 1 >= function.code.len) return error.ExpectedSetListBatchNumberInstruction;
                    instructions_to_skip += 1;
                }
            },
            //.closure => {},
            .vararg => {
                const vararg_inst: Instruction.VarArg = @bitCast(instruction);
                const num_returns = vararg_inst.getNumReturnValues();
                if (num_returns == null) {
                    try checkopenop_next(function, i);
                    // TODO how does lua avoid overflow here?
                    //try checkreg(function, a + num_returns.? - 1);
                } else {
                    try checkreg(function, a + num_returns.? - 1);
                }
            },
            else => {},
        }
    }
    return function.code[last_instruction_that_changed_reg];
}
