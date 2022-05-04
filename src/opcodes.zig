const std = @import("std");
const zua = @import("zua.zig");
const Token = zua.lex.Token;

// From lopcodes.h:
//
// We assume that instructions are unsigned numbers.
// All instructions have an opcode in the first 6 bits.
// Instructions can have the following fields:
//  'A': 8 bits
//  'B': 9 bits
//  'C': 9 bits
//  'Bx': 18 bits ('B' and 'C' together)
//  'sBx': signed Bx
//
// A signed argument is represented in excess K; that is, the number
// value is the unsigned value minus K. K is exactly the maximum value
// for that argument (so that -max is represented by 0, and +max is
// represented by 2*max), which is half the maximum for the corresponding
// unsigned argument.

pub const OpCode = enum(u6) {
    // TODO: rest of the opcodes
    move = 0,
    loadk = 1,
    loadbool = 2,
    loadnil = 3,
    getglobal = 5,
    gettable = 6,
    setglobal = 7,
    settable = 9,
    newtable = 10,
    self = 11,
    add = 12,
    sub = 13,
    mul = 14,
    div = 15,
    mod = 16,
    pow = 17,
    unm = 18,
    not = 19,
    len = 20,
    concat = 21,
    jmp = 22,
    call = 28,
    tailcall = 29,
    @"return" = 30,
    setlist = 34,
    vararg = 37,

    pub fn InstructionType(op: OpCode) type {
        return switch (op) {
            .move => Instruction.Move,
            .loadk => Instruction.LoadK,
            .loadbool => Instruction.LoadBool,
            .loadnil => Instruction.LoadNil,
            .getglobal => Instruction.GetGlobal,
            .gettable => Instruction.GetTable,
            .setglobal => Instruction.SetGlobal,
            .settable => Instruction.SetTable,
            .newtable => Instruction.NewTable,
            .self => Instruction.Self,
            .add, .sub, .mul, .div, .mod, .pow => Instruction.BinaryMath,
            .unm => Instruction.UnaryMinus,
            .not => Instruction.Not,
            .len => Instruction.Length,
            .concat => Instruction.Concat,
            .jmp => Instruction.Jump,
            .call, .tailcall => Instruction.Call,
            .@"return" => Instruction.Return,
            .setlist => Instruction.SetList,
            .vararg => Instruction.VarArg,
        };
    }

    pub const OpMode = enum {
        iABC,
        iABx,
        iAsBx,
    };

    // A mapping of OpCode -> OpMode
    const op_modes = blk: {
        const max_fields = std.math.maxInt(@typeInfo(OpCode).Enum.tag_type);
        var array: [max_fields]OpMode = undefined;
        for (@typeInfo(OpCode).Enum.fields) |field| {
            const Type = @field(OpCode, field.name).InstructionType();
            const mode: OpMode = switch (@typeInfo(Type).Struct.fields[0].field_type) {
                Instruction.ABC => .iABC,
                Instruction.ABx => .iABx,
                Instruction.AsBx => .iAsBx,
                else => unreachable,
            };
            array[field.value] = mode;
        }
        break :blk array;
    };

    pub fn getOpMode(self: OpCode) OpMode {
        return op_modes[@enumToInt(self)];
    }

    pub const OpArgMask = enum {
        NotUsed, // N
        Used, // U
        RegisterOrJumpOffset, // R
        ConstantOrRegisterConstant, // K
    };

    pub const OpMeta = struct {
        b_mode: OpArgMask,
        c_mode: OpArgMask,
        sets_register_in_a: bool,
        test_t_mode: bool,
    };

    const op_meta = blk: {
        const max_fields = std.math.maxInt(@typeInfo(OpCode).Enum.tag_type);
        var array: [max_fields]*const OpMeta = undefined;
        for (@typeInfo(OpCode).Enum.fields) |field| {
            const Type = @field(OpCode, field.name).InstructionType();
            const meta = &@field(Type, "meta");
            array[field.value] = meta;
        }
        break :blk array;
    };

    pub fn getBMode(self: OpCode) OpArgMask {
        return op_meta[@enumToInt(self)].b_mode;
    }

    pub fn getCMode(self: OpCode) OpArgMask {
        return op_meta[@enumToInt(self)].c_mode;
    }

    /// If true, the instruction will set the register index specified in its `A` value
    pub fn setsRegisterInA(self: OpCode) bool {
        return op_meta[@enumToInt(self)].sets_register_in_a;
    }

    // TODO rename
    /// operator is a test
    pub fn testTMode(self: OpCode) bool {
        return op_meta[@enumToInt(self)].test_t_mode;
    }
};

// R(x) - register
// Kst(x) - constant (in constant table)
// RK(x) == if ISK(x) then Kst(INDEXK(x)) else R(x)

/// SIZE_B define equivalent (lopcodes.h)
const bit_size_b = 9;
/// BITRK define equivalent (lopcodes.h)
const rk_bit_mask_constant: u9 = 1 << (bit_size_b - 1);
/// MAXINDEXRK define equivalent (lopcodes.h)
pub const rk_max_constant_index: u9 = rk_bit_mask_constant - 1;

/// ISK macro equivalent (lopcodes.h)
pub fn rkIsConstant(val: u9) bool {
    return val & rk_bit_mask_constant != 0;
}

/// INDEXK macro equivalent (lopcodes.h)
pub fn rkGetConstantIndex(val: u9) u9 {
    return val & ~rk_bit_mask_constant;
}

/// RKASK macro equivalent (lopcodes.h)
pub fn constantIndexToRK(val: u9) u9 {
    return val | rk_bit_mask_constant;
}

/// To be bit-casted depending on the op field
pub const Instruction = packed struct {
    op: OpCode,
    a: u8,
    fields: u18,

    pub const ABC = packed struct {
        op: OpCode,
        a: u8,
        c: u9,
        b: u9,

        pub fn init(op: OpCode, a: u8, b: u9, c: u9) Instruction.ABC {
            return .{
                .op = op,
                .a = a,
                .b = b,
                .c = c,
            };
        }

        pub const max_a = std.math.maxInt(u8);
        pub const max_b = std.math.maxInt(u9);
        pub const max_c = std.math.maxInt(u9);
    };

    pub const ABx = packed struct {
        op: OpCode,
        a: u8,
        bx: u18,

        pub fn init(op: OpCode, a: u8, bx: u18) Instruction.ABx {
            return .{
                .op = op,
                .a = a,
                .bx = bx,
            };
        }

        pub const max_bx = std.math.maxInt(u18);
    };

    pub const AsBx = packed struct {
        op: OpCode,
        a: u8,
        /// Underscore in the name to make it hard to accidentally use this field directly.
        /// Stored as unsigned for binary compatibility
        _bx: u18,

        pub fn init(op: OpCode, a: u8, sbx: i18) Instruction.AsBx {
            return .{
                .op = op,
                .a = a,
                ._bx = signedBxToUnsigned(sbx),
            };
        }

        pub fn getSignedBx(self: *const Instruction.AsBx) i18 {
            return unsignedBxToSigned(self._bx);
        }

        pub fn setSignedBx(self: *Instruction.AsBx, val: i18) void {
            self._bx = signedBxToUnsigned(val);
        }

        pub const max_sbx = std.math.maxInt(i18);
        // Not std.math.minInt because of the subtraction stuff
        pub const min_sbx = -max_sbx;

        pub fn unsignedBxToSigned(Bx: u18) i18 {
            const fitting_int = std.math.IntFittingRange(min_sbx, ABx.max_bx);
            return @intCast(i18, @intCast(fitting_int, Bx) - max_sbx);
        }

        pub fn signedBxToUnsigned(sBx: i18) u18 {
            const fitting_int = std.math.IntFittingRange(min_sbx, ABx.max_bx);
            return @intCast(u18, @intCast(fitting_int, sBx) + max_sbx);
        }
    };

    /// R(A) := R(B)
    pub const Move = packed struct {
        /// really only A B
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .RegisterOrJumpOffset,
            .c_mode = .NotUsed,
            .sets_register_in_a = true,
            .test_t_mode = false,
        };
    };

    /// R(A) := Kst(Bx)
    pub const LoadK = packed struct {
        instruction: Instruction.ABx,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .ConstantOrRegisterConstant,
            .c_mode = .NotUsed,
            .sets_register_in_a = true,
            .test_t_mode = false,
        };

        pub fn init(result_reg: u8, constant_index: u18) LoadK {
            return .{
                .instruction = Instruction.ABx.init(
                    .loadk,
                    result_reg,
                    constant_index,
                ),
            };
        }
    };

    /// R(A) := (Bool)B; if (C) pc++
    pub const LoadBool = packed struct {
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .Used,
            .c_mode = .Used,
            .sets_register_in_a = true,
            .test_t_mode = false,
        };

        pub fn init(result_reg: u8, val: bool, does_jump: bool) LoadBool {
            return .{
                .instruction = Instruction.ABC.init(
                    .loadbool,
                    result_reg,
                    @boolToInt(val),
                    @boolToInt(does_jump),
                ),
            };
        }

        pub fn doesJump(self: *const LoadBool) bool {
            return self.instruction.c != 0;
        }
    };

    /// R(A) := ... := R(B) := nil
    pub const LoadNil = packed struct {
        /// really only A B
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .RegisterOrJumpOffset,
            .c_mode = .NotUsed,
            .sets_register_in_a = true,
            .test_t_mode = false,
        };

        /// Note: reg_range_start and reg_range_end are both inclusive,
        /// so for setting a single register start and end should
        /// be equal
        pub fn init(reg_range_start: u8, reg_range_end: u9) LoadNil {
            std.debug.assert(reg_range_end >= reg_range_start);
            return .{
                .instruction = Instruction.ABC.init(
                    .loadnil,
                    reg_range_start,
                    reg_range_end,
                    0,
                ),
            };
        }

        pub fn assignsToMultipleRegisters(self: LoadNil) bool {
            return self.instruction.a != @truncate(u8, self.instruction.b);
        }

        pub fn willAssignToRegister(self: LoadNil, reg: u8) bool {
            return reg >= self.instruction.a and reg <= self.instruction.b;
        }
    };

    /// R(A) := Gbl[Kst(Bx)]
    pub const GetGlobal = packed struct {
        instruction: Instruction.ABx,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .ConstantOrRegisterConstant,
            .c_mode = .NotUsed,
            .sets_register_in_a = true,
            .test_t_mode = false,
        };

        pub fn init(result_reg: u8, name_constant_index: u18) GetGlobal {
            return .{
                .instruction = .{
                    .op = .getglobal,
                    .a = result_reg,
                    .bx = name_constant_index,
                },
            };
        }
    };

    /// R(A) := R(B)[RK(C)]
    pub const GetTable = packed struct {
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .RegisterOrJumpOffset,
            .c_mode = .ConstantOrRegisterConstant,
            .sets_register_in_a = true,
            .test_t_mode = false,
        };

        pub fn init(result_reg: u8, table_reg: u9, key_rk: u9) GetTable {
            return .{
                .instruction = .{
                    .op = .gettable,
                    .a = result_reg,
                    .b = table_reg,
                    .c = key_rk,
                },
            };
        }
    };

    /// Gbl[Kst(Bx)] := R(A)
    pub const SetGlobal = packed struct {
        instruction: Instruction.ABx,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .ConstantOrRegisterConstant,
            .c_mode = .NotUsed,
            .sets_register_in_a = false,
            .test_t_mode = false,
        };

        pub fn init(name_constant_index: u18, source_reg: u8) SetGlobal {
            return .{
                .instruction = .{
                    .op = .setglobal,
                    .a = source_reg,
                    .bx = name_constant_index,
                },
            };
        }
    };

    /// R(A)[RK(B)] := RK(C)
    pub const SetTable = packed struct {
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .ConstantOrRegisterConstant,
            .c_mode = .ConstantOrRegisterConstant,
            .sets_register_in_a = false,
            .test_t_mode = false,
        };

        pub fn init(table_reg: u8, key_rk: u9, val_rk: u9) SetTable {
            return .{
                .instruction = Instruction.ABC.init(
                    .settable,
                    table_reg,
                    key_rk,
                    val_rk,
                ),
            };
        }
    };

    /// R(A) := {} (size = B,C)
    pub const NewTable = packed struct {
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .Used,
            .c_mode = .Used,
            .sets_register_in_a = true,
            .test_t_mode = false,
        };

        pub fn setArraySize(self: *NewTable, num: zua.object.FloatingPointByteIntType) void {
            self.instruction.b = @intCast(u9, zua.object.intToFloatingPointByte(num));
        }

        pub fn setTableSize(self: *NewTable, num: zua.object.FloatingPointByteIntType) void {
            self.instruction.c = @intCast(u9, zua.object.intToFloatingPointByte(num));
        }
    };

    /// R(A+1) := R(B); R(A) := R(B)[RK(C)]
    pub const Self = packed struct {
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .RegisterOrJumpOffset,
            .c_mode = .ConstantOrRegisterConstant,
            .sets_register_in_a = true,
            .test_t_mode = false,
        };

        /// Stores the function (gotten from table[key]) in `setup_reg_start`
        /// and the table itself in `setup_reg_start + 1`
        pub fn init(setup_reg_start: u8, table_reg: u9, key_rk: u9) Self {
            return .{ .instruction = .{
                .op = .self,
                .a = setup_reg_start,
                .b = table_reg,
                .c = key_rk,
            } };
        }
    };

    /// R(A) := RK(B) <operation> RK(C)
    pub const BinaryMath = packed struct {
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .ConstantOrRegisterConstant,
            .c_mode = .ConstantOrRegisterConstant,
            .sets_register_in_a = true,
            .test_t_mode = false,
        };

        pub fn init(op: OpCode, result_reg: u8, left_rk: u9, right_rk: u9) BinaryMath {
            return .{
                .instruction = Instruction.ABC.init(
                    op,
                    result_reg,
                    left_rk,
                    right_rk,
                ),
            };
        }

        pub fn tokenToOpCode(token: Token) OpCode {
            return switch (token.char.?) {
                '+' => .add,
                '-' => .sub,
                '*' => .mul,
                '/' => .div,
                '%' => .mod,
                '^' => .pow,
                else => unreachable,
            };
        }
    };

    /// R(A) := -R(B)
    pub const UnaryMinus = packed struct {
        /// really only A B
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .RegisterOrJumpOffset,
            .c_mode = .NotUsed,
            .sets_register_in_a = true,
            .test_t_mode = false,
        };
    };

    /// R(A) := not R(B)
    pub const Not = packed struct {
        /// really only A B
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .RegisterOrJumpOffset,
            .c_mode = .NotUsed,
            .sets_register_in_a = true,
            .test_t_mode = false,
        };

        pub fn init(result_reg: u8, value_reg: u9) Not {
            return .{
                .instruction = Instruction.ABC.init(
                    .not,
                    result_reg,
                    value_reg,
                    0,
                ),
            };
        }
    };

    /// R(A) := length of R(B)
    pub const Length = packed struct {
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .RegisterOrJumpOffset,
            .c_mode = .NotUsed,
            .sets_register_in_a = true,
            .test_t_mode = false,
        };
    };

    /// R(A) := R(B).. ... ..R(C)
    pub const Concat = packed struct {
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .RegisterOrJumpOffset,
            .c_mode = .RegisterOrJumpOffset,
            .sets_register_in_a = true,
            .test_t_mode = false,
        };

        pub fn init(result_reg: u8, start_reg: u9, end_reg: u9) Concat {
            std.debug.assert(end_reg > start_reg);
            return .{ .instruction = .{
                .op = .concat,
                .a = result_reg,
                .b = start_reg,
                .c = end_reg,
            } };
        }

        pub fn getStartReg(self: Concat) u9 {
            return self.instruction.b;
        }

        pub fn setStartReg(self: *Concat, start_reg: u9) void {
            self.instruction.b = start_reg;
        }

        pub fn getEndReg(self: Concat) u9 {
            return self.instruction.c;
        }

        pub fn setEndReg(self: *Concat, end_reg: u9) void {
            self.instruction.c = end_reg;
        }

        pub fn numConcattedValues(self: Concat) u9 {
            return (self.instruction.c - self.instruction.b) + 1;
        }
    };

    /// pc+=sBx
    pub const Jump = packed struct {
        /// really only sBx
        instruction: Instruction.AsBx,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .RegisterOrJumpOffset,
            .c_mode = .NotUsed,
            .sets_register_in_a = false,
            .test_t_mode = false,
        };

        pub fn init(offset: ?i18) Jump {
            const instruction = Instruction.AsBx.init(.jmp, 0, 0);
            var jmp = @bitCast(Jump, instruction);
            jmp.setOffset(offset);
            return jmp;
        }

        /// -1 can be used to represent 'no jump' here because that would mean that an instruction
        /// is jumping to itself, which is obviously not something we'd ever want to do
        const no_jump = -1;

        pub fn getOffset(self: *const Jump) ?i18 {
            const sbx = self.instruction.getSignedBx();
            if (sbx == no_jump) return null;
            return sbx;
        }

        pub fn setOffset(self: *Jump, offset: ?i18) void {
            self.instruction.setSignedBx(offset orelse no_jump);
        }

        pub fn offsetToAbsolute(pc: usize, offset: i18) usize {
            // As I understand it, the + 1 here is necessary to emulate
            // the implicit pc++ when evaluating the jmp instruction itself.
            // That is, if there's bytecode like:
            //  1 jmp 1
            //  2 something
            //  3 something
            // then the jmp would offset pc by 1, but then it'd get offset
            // again by the natural movement of the pc after evaluating
            // any instruction, so we'd actually end up at 3
            //
            // TODO better understand why this + 1 exists/verify the above
            return @intCast(usize, @intCast(isize, pc + 1) + offset);
        }
    };

    /// Used for both call and tailcall opcodes
    /// call: R(A), ... ,R(A+C-2) := R(A)(R(A+1), ... ,R(A+B-1))
    /// tailcall: return R(A)(R(A+1), ... ,R(A+B-1))
    pub const Call = packed struct {
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .Used,
            .c_mode = .Used,
            .sets_register_in_a = true,
            .test_t_mode = false,
        };

        pub fn init(result_reg_start: u8, num_params: u9, num_return_values: ?u9) Call {
            const c_val = if (num_return_values != null) num_return_values.? + 1 else 0;
            return .{
                .instruction = Instruction.ABC.init(
                    // TODO: This is always being .call is kinda weird, but it happens to work with
                    //       how the Lua compiler handles call/tailcall (they all start as call
                    //       and then get converted to tailcall)
                    .call,
                    result_reg_start,
                    num_params + 1,
                    c_val,
                ),
            };
        }

        pub fn getNumParams(self: Call) u9 {
            return self.instruction.b - 1;
        }

        // 'base register for call' is what it's called in lparser.c:643
        pub fn setResultRegStart(self: *Call, base_reg: u8) void {
            self.instruction.a = base_reg;
        }

        pub fn getResultRegStart(self: *const Call) u8 {
            return self.instruction.a;
        }

        pub fn getResultRegEnd(self: *const Call) ?u9 {
            if (self.getNumReturnValues()) |return_vals| {
                if (return_vals > 0) {
                    return self.getResultRegStart() + return_vals - 1;
                } else {
                    return null;
                }
            } else {
                return null;
            }
        }

        pub fn setNumReturnValues(self: *Call, num_return_values: ?u9) void {
            if (num_return_values) |v| {
                self.instruction.c = v + 1;
            } else {
                self.instruction.c = 0;
            }
        }

        pub fn getNumReturnValues(self: *const Call) ?u9 {
            if (self.isMultipleReturns()) return null;
            return self.instruction.c - 1;
        }

        pub fn isMultipleReturns(self: *const Call) bool {
            return self.instruction.c == 0;
        }
    };

    /// return R(A), ... ,R(A+B-2)
    /// if (B == 0) then return up to `top'
    pub const Return = packed struct {
        /// really only A B
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .Used,
            .c_mode = .NotUsed,
            .sets_register_in_a = false,
            .test_t_mode = false,
        };

        pub fn init(first_return_value_register: u8, num_return_values: ?u9) Return {
            const b_val = if (num_return_values != null) num_return_values.? + 1 else 0;
            return .{
                .instruction = Instruction.ABC.init(
                    .@"return",
                    first_return_value_register,
                    b_val,
                    0,
                ),
            };
        }

        pub fn getFirstReturnValueRegister(self: *const Return) u8 {
            return self.instruction.a;
        }

        pub fn setFirstReturnValueRegister(self: *Return, first_return_value_register: u8) void {
            self.instruction.a = first_return_value_register;
        }

        pub fn setNumReturnValues(self: *Return, num_return_values: ?u9) void {
            if (num_return_values) |v| {
                self.instruction.b = v + 1;
            } else {
                self.instruction.b = 0;
            }
        }

        pub fn getNumReturnValues(self: *const Return) ?u9 {
            if (self.isMultipleReturns()) return null;
            return self.instruction.b - 1;
        }

        pub fn isMultipleReturns(self: *const Return) bool {
            return self.instruction.b == 0;
        }
    };

    /// R(A)[(C-1)*FPF+i] := R(A+i), 1 <= i <= B
    /// if (B == 0) then B = `top'
    /// if (C == 0) then next `instruction' is real C
    pub const SetList = packed struct {
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .Used,
            .c_mode = .Used,
            .sets_register_in_a = false,
            .test_t_mode = false,
        };

        pub const batch_num_in_next_instruction = 0;

        /// Note: If the resulting SetList's c value is `batch_num_in_next_instruction`
        /// (or `isBatchNumberStoredInNextInstruction` returns true), then this instruction
        /// will need an additional instruction after it that contains the full batch number
        /// (as a bare u32)
        pub fn init(table_reg: u8, num_values: usize, to_store: ?usize) SetList {
            const flush_batch_num: usize = SetList.numValuesToFlushBatchNum(num_values);
            const num_values_in_batch: u9 = if (to_store == null) 0 else @intCast(u9, to_store.?);
            const c_val = if (flush_batch_num <= Instruction.ABC.max_c)
                @intCast(u9, flush_batch_num)
            else
                batch_num_in_next_instruction;

            return .{ .instruction = .{
                .op = .setlist,
                .a = table_reg,
                .b = num_values_in_batch,
                .c = c_val,
            } };
        }

        pub fn numValuesToFlushBatchNum(num_values: usize) usize {
            return (num_values - 1) / Instruction.SetList.fields_per_flush + 1;
        }

        pub fn isBatchNumberStoredInNextInstruction(self: *const SetList) bool {
            return self.instruction.c == 0;
        }

        /// equivalent to LFIELDS_PER_FLUSH from lopcodes.h
        pub const fields_per_flush = 50;
    };

    /// R(A), R(A+1), ..., R(A+B-1) = vararg
    pub const VarArg = packed struct {
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .Used,
            .c_mode = .NotUsed,
            .sets_register_in_a = true,
            .test_t_mode = false,
        };

        pub fn init(first_return_value_register: u8, num_return_values: ?u9) VarArg {
            const b_val = if (num_return_values != null) num_return_values.? + 1 else 0;
            return .{
                .instruction = Instruction.ABC.init(
                    .vararg,
                    first_return_value_register,
                    b_val,
                    0,
                ),
            };
        }

        pub fn getFirstReturnValueRegister(self: *const VarArg) u8 {
            return self.instruction.a;
        }

        pub fn setFirstReturnValueRegister(self: *VarArg, first_return_value_register: u8) void {
            self.instruction.a = first_return_value_register;
        }

        pub fn setNumReturnValues(self: *VarArg, num_return_values: ?u9) void {
            if (num_return_values) |v| {
                self.instruction.b = v + 1;
            } else {
                self.instruction.b = 0;
            }
        }

        pub fn getNumReturnValues(self: *const VarArg) ?u9 {
            if (self.isMultipleReturns()) return null;
            return self.instruction.b - 1;
        }

        pub fn isMultipleReturns(self: *const VarArg) bool {
            return self.instruction.b == 0;
        }
    };
};

test "sBx" {
    try std.testing.expectEqual(
        @as(i18, Instruction.AsBx.min_sbx),
        Instruction.AsBx.unsignedBxToSigned(0),
    );
    const max_sbx_as_bx = Instruction.AsBx.signedBxToUnsigned(Instruction.AsBx.max_sbx);
    try std.testing.expectEqual(
        @as(i18, Instruction.AsBx.max_sbx),
        Instruction.AsBx.unsignedBxToSigned(max_sbx_as_bx),
    );
}
