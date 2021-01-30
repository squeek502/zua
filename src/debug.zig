const std = @import("std");
const zua = @import("zua.zig");
const Function = zua.object.Function;
const OpCode = zua.opcodes.OpCode;
const Instruction = zua.opcodes.Instruction;
const InstructionABC = zua.opcodes.InstructionABC;
const InstructionABx = zua.opcodes.InstructionABx;
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
            try checkreg(function, @intCast(usize, val));
        },
        .ConstantOrRegisterConstant => {
            const actual_size_val = @intCast(u9, val);
            const is_constant = zua.opcodes.isConstant(actual_size_val);
            if (is_constant) {
                if (zua.opcodes.getConstantIndex(actual_size_val) >= function.constants.len) return error.ConstantIndexOutOfRange;
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
        //.setlist,
        => {
            const b = @bitCast(InstructionABC, instruction).b;
            if (b != 0) return error.InvalidInstructionAfterOpenCall;
        },
        else => return error.InvalidInstructionAfterOpenCall,
    }
}

fn symbexec(function: *const Function, reg: ?usize) !Instruction {
    try precheck(function);
    // defaults to final return (a 'neutral' instruction)
    var last_instruction_that_changed_reg: usize = function.code.len - 1;
    for (function.code) |instruction, i| {
        const a = instruction.a;
        var b: i32 = 0;
        var c: i32 = 0;
        try checkreg(function, a);
        switch (instruction.op.getOpMode()) {
            .iABC => {
                const instructionABC = @bitCast(InstructionABC, instruction);
                b = instructionABC.b;
                c = instructionABC.c;
                try checkArgMode(function, b, instruction.op.getBMode());
                try checkArgMode(function, c, instruction.op.getCMode());
            },
            .iABx => {
                const instructionABx = @bitCast(InstructionABx, instruction);
                b = instructionABx.bx;
                if (instruction.op.getBMode() == .ConstantOrRegisterConstant) {
                    if (b >= function.constants.len) return error.ConstantIndexOutOfRange;
                }
            },
            .iAsBx => {
                const instructionABx = @bitCast(InstructionABx, instruction);
                b = instructionABx.getSignedBx();
                if (instruction.op.getBMode() == .RegisterOrJumpOffset) {
                    @panic("TODO");
                }
            },
        }
        if (instruction.op.testAMode()) {
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
                if (c == 1) { // does it jump?
                    if (i + 2 >= function.code.len) return error.ImpossibleLoadBoolInstructionPlacement;
                    //const next_instruction = function.code[i+1];
                    //const check = next_instruction.op != .setlist or next_instruction.(somehow get arg c) != 0;
                    //if (!check) return error.ImpossibleInstructionAfterLoadBool;
                }
            },
            .loadnil => {
                if (reg != null and a <= reg.? and reg.? <= b) {
                    last_instruction_that_changed_reg = i;
                }
            },
            //.getupval, .setupval => {},
            .getglobal => {
                const constant = function.constants[@intCast(usize, b)];
                if (constant != .string) return error.ExpectedStringForGetOrSetGlobal;
            },
            //.self => {},
            //.concat => {},
            //.tforloop => {},
            //.forloop, .forprep => {},
            //.jmp => {},
            .call => {
                if (b != 0) {
                    try checkreg(function, @intCast(usize, a + b - 1));
                }
                const num_returns: i32 = c - 1;
                if (num_returns == -1) { // TODO LUA_MULTRET
                    try checkopenop_next(function, i);
                } else if (num_returns != 0) {
                    try checkreg(function, @intCast(usize, a + num_returns - 1));
                }
                if (reg != null and reg.? >= a) {
                    last_instruction_that_changed_reg = i;
                }
            },
            .@"return" => {
                const num_returns: i32 = b - 1;
                if (num_returns > 0) {
                    try checkreg(function, @intCast(usize, a + b - 1));
                }
            },
            //.setlist => {},
            //.closure => {},
            //.vararg => {},
            else => {},
        }
    }
    return function.code[last_instruction_that_changed_reg];
}
