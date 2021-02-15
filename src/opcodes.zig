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

pub const OpCode = packed enum(u6) {
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
    len = 20,
    concat = 21,
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
            .len => Instruction.Length,
            .concat => Instruction.Concat,
            .call => Instruction.Call,
            .tailcall => Instruction.TailCall,
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
    const op_modes = comptime blk: {
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
        test_a_mode: bool,
        test_t_mode: bool,
    };

    const op_meta = comptime blk: {
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

    // TODO rename
    /// instruction set register A
    pub fn testAMode(self: OpCode) bool {
        return op_meta[@enumToInt(self)].test_a_mode;
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
const bit_mask_constant: u9 = 1 << (bit_size_b - 1);
/// MAXINDEXRK define equivalent (lopcodes.h)
pub const max_constant_index: u9 = bit_mask_constant - 1;

/// ISK macro equivalent (lopcodes.h)
pub fn isConstant(val: u9) bool {
    return val & bit_mask_constant != 0;
}

/// INDEXK macro equivalent (lopcodes.h)
pub fn getConstantIndex(val: u9) u9 {
    return val & ~bit_mask_constant;
}

/// RKASK macro equivalent (lopcodes.h)
pub fn constantIndexToRK(val: u9) u9 {
    return val | bit_mask_constant;
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
                ._bx = bx,
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
            comptime const fitting_int = std.math.IntFittingRange(min_sbx, ABx.max_bx);
            return @intCast(i18, @intCast(fitting_int, Bx) - max_sbx);
        }

        pub fn signedBxToUnsigned(sBx: i18) u18 {
            comptime const fitting_int = std.math.IntFittingRange(min_sbx, ABx.max_bx);
            return @intCast(u18, @intCast(fitting_int, sBx) + max_sbx);
        }
    };

    pub const Move = packed struct {
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .RegisterOrJumpOffset,
            .c_mode = .NotUsed,
            .test_a_mode = true,
            .test_t_mode = false,
        };
    };

    pub const LoadK = packed struct {
        instruction: Instruction.ABx,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .ConstantOrRegisterConstant,
            .c_mode = .NotUsed,
            .test_a_mode = true,
            .test_t_mode = false,
        };
    };

    pub const LoadBool = packed struct {
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .Used,
            .c_mode = .Used,
            .test_a_mode = true,
            .test_t_mode = false,
        };

        pub fn init(reg: u8, val: bool, does_jump: bool) LoadBool {
            return .{
                .instruction = Instruction.ABC.init(
                    .loadbool,
                    reg,
                    @boolToInt(val),
                    @boolToInt(does_jump),
                ),
            };
        }

        pub fn doesJump(self: *const LoadBool) bool {
            return self.instruction.c == 1;
        }
    };

    pub const LoadNil = packed struct {
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .RegisterOrJumpOffset,
            .c_mode = .NotUsed,
            .test_a_mode = true,
            .test_t_mode = false,
        };
    };

    pub const GetGlobal = packed struct {
        instruction: Instruction.ABx,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .ConstantOrRegisterConstant,
            .c_mode = .NotUsed,
            .test_a_mode = true,
            .test_t_mode = false,
        };
    };

    pub const GetTable = packed struct {
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .RegisterOrJumpOffset,
            .c_mode = .ConstantOrRegisterConstant,
            .test_a_mode = true,
            .test_t_mode = false,
        };
    };

    pub const SetGlobal = packed struct {
        instruction: Instruction.ABx,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .ConstantOrRegisterConstant,
            .c_mode = .NotUsed,
            .test_a_mode = false,
            .test_t_mode = false,
        };
    };

    pub const SetTable = packed struct {
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .ConstantOrRegisterConstant,
            .c_mode = .ConstantOrRegisterConstant,
            .test_a_mode = false,
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

    pub const NewTable = packed struct {
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .Used,
            .c_mode = .Used,
            .test_a_mode = true,
            .test_t_mode = false,
        };

        pub fn setArraySize(self: *NewTable, num: zua.object.FloatingPointByteIntType) void {
            self.instruction.b = @intCast(u9, zua.object.intToFloatingPointByte(num));
        }

        pub fn setTableSize(self: *NewTable, num: zua.object.FloatingPointByteIntType) void {
            self.instruction.c = @intCast(u9, zua.object.intToFloatingPointByte(num));
        }
    };

    pub const Self = packed struct {
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .RegisterOrJumpOffset,
            .c_mode = .ConstantOrRegisterConstant,
            .test_a_mode = true,
            .test_t_mode = false,
        };
    };

    pub const BinaryMath = packed struct {
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .ConstantOrRegisterConstant,
            .c_mode = .ConstantOrRegisterConstant,
            .test_a_mode = true,
            .test_t_mode = false,
        };

        // TODO what is `a`?
        pub fn init(op: OpCode, a: u8, left_rk: u9, right_rk: u9) BinaryMath {
            return .{
                .instruction = Instruction.ABC.init(
                    op,
                    a,
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

    pub const UnaryMinus = packed struct {
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .RegisterOrJumpOffset,
            .c_mode = .NotUsed,
            .test_a_mode = true,
            .test_t_mode = false,
        };
    };

    pub const Length = packed struct {
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .RegisterOrJumpOffset,
            .c_mode = .NotUsed,
            .test_a_mode = true,
            .test_t_mode = false,
        };
    };

    pub const Concat = packed struct {
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .RegisterOrJumpOffset,
            .c_mode = .RegisterOrJumpOffset,
            .test_a_mode = true,
            .test_t_mode = false,
        };
    };

    pub const Call = packed struct {
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .Used,
            .c_mode = .Used,
            .test_a_mode = true,
            .test_t_mode = false,
        };

        pub fn init(base: u8, num_params: u9, num_return_values: ?u9) Call {
            const c_val = if (num_return_values != null) num_return_values.? + 1 else 0;
            return .{
                .instruction = Instruction.ABC.init(
                    .call,
                    base,
                    num_params + 1,
                    c_val,
                ),
            };
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

    pub const TailCall = packed struct {
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .Used,
            .c_mode = .Used,
            .test_a_mode = true,
            .test_t_mode = false,
        };
    };

    pub const Return = packed struct {
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .Used,
            .c_mode = .NotUsed,
            .test_a_mode = false,
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

    pub const SetList = packed struct {
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .Used,
            .c_mode = .Used,
            .test_a_mode = false,
            .test_t_mode = false,
        };

        /// equivalent to LFIELDS_PER_FLUSH from lopcodes.h
        pub const fields_per_flush = 50;
    };

    pub const VarArg = packed struct {
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .Used,
            .c_mode = .NotUsed,
            .test_a_mode = true,
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
    std.testing.expectEqual(
        @as(i18, Instruction.AsBx.min_sbx),
        Instruction.AsBx.unsignedBxToSigned(0),
    );
    const max_sbx_as_bx = Instruction.AsBx.signedBxToUnsigned(Instruction.AsBx.max_sbx);
    std.testing.expectEqual(
        @as(i18, Instruction.AsBx.max_sbx),
        Instruction.AsBx.unsignedBxToSigned(max_sbx_as_bx),
    );
}
