const std = @import("std");

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
    loadk = 1,
    loadbool = 2,
    loadnil = 3,
    getglobal = 5,
    call = 28,
    @"return" = 30,

    pub const OpMode = enum {
        iABC,
        iABx,
        iAsBx,
    };

    pub fn getOpMode(self: OpCode) OpMode {
        return switch (self) {
            .loadk, .getglobal => .iABx,
            .call, .@"return", .loadbool, .loadnil => .iABC,
        };
    }

    pub const OpArgMask = enum {
        NotUsed, // N
        Used, // U
        RegisterOrJumpOffset, // R
        ConstantOrRegisterConstant, // K
    };

    pub fn getBMode(self: OpCode) OpArgMask {
        return switch (self) {
            .loadnil => .RegisterOrJumpOffset,
            .loadk, .getglobal => .ConstantOrRegisterConstant,
            .call, .@"return", .loadbool => .Used,
        };
    }

    pub fn getCMode(self: OpCode) OpArgMask {
        return switch (self) {
            .loadk, .getglobal => .NotUsed,
            .call, .loadbool => .Used,
            .@"return", .loadnil => .NotUsed,
        };
    }

    // TODO rename
    /// instruction set register A
    pub fn testAMode(self: OpCode) bool {
        return switch (self) {
            .loadk => true,
            .loadbool => true,
            .loadnil => true,
            .getglobal => true,
            .call => true,
            .@"return" => false,
        };
    }

    // TODO rename
    /// operator is a test
    pub fn testTMode(self: OpCode) bool {
        return switch (self) {
            .loadk,
            .loadbool,
            .loadnil,
            .getglobal,
            .call,
            .@"return",
            => false,
        };
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
    return val & bit_mask_constant == 1;
}

/// INDEXK macro equivalent (lopcodes.h)
pub fn getConstantIndex(val: u9) u9 {
    return val & ~bit_mask_constant;
}

/// RKASK macro equivalent (lopcodes.h)
pub fn constantIndexToRK(val: u9) u9 {
    return val | bit_mask_constant;
}

// TODO use a packed union for fields (iABC and iBx as fields of a packed union)
// once bit packing is implemented for packed unions

/// To be bit-casted depending on the op field
pub const Instruction = packed struct {
    op: OpCode,
    a: u8,
    fields: u18,
};

pub const InstructionABC = packed struct {
    op: OpCode,
    a: u8,
    c: u9,
    b: u9,

    pub fn init(op: OpCode, a: u8, b: u9, c: u9) InstructionABC {
        return .{
            .op = op,
            .a = a,
            .b = b,
            .c = c,
        };
    }
};

pub const InstructionABx = packed struct {
    op: OpCode,
    a: u8,
    bx: u18,

    pub fn init(op: OpCode, a: u8, bx: u18) InstructionABx {
        return .{
            .op = op,
            .a = a,
            .bx = bx,
        };
    }

    pub fn initSigned(op: OpCode, a: u8, sbx: i18) InstructionABx {
        return init(op, a, signedBxToUnsigned(sbx));
    }

    pub fn getSignedBx(self: *const InstructionABx) i18 {
        return unsignedBxToSigned(self.bx);
    }

    pub fn setSignedBx(self: *InstructionABx, val: i18) void {
        self.setBx(signedBxToUnsigned(val));
    }

    const max_bx = std.math.maxInt(u18);
    const max_sbx = std.math.maxInt(i18);
    /// Not std.math.minInt because of the subtraction stuff
    const min_sbx = -max_sbx;

    pub fn unsignedBxToSigned(Bx: u18) i18 {
        comptime const fitting_int = std.math.IntFittingRange(min_sbx, max_bx);
        return @intCast(i18, @intCast(fitting_int, Bx) - max_sbx);
    }

    pub fn signedBxToUnsigned(sBx: i18) u18 {
        comptime const fitting_int = std.math.IntFittingRange(min_sbx, max_bx);
        return @intCast(u18, @intCast(fitting_int, sBx) + max_sbx);
    }
};

test "sBx" {
    std.testing.expectEqual(
        @as(i18, InstructionABx.min_sbx),
        InstructionABx.unsignedBxToSigned(0),
    );
    const max_sbx_as_bx = InstructionABx.signedBxToUnsigned(InstructionABx.max_sbx);
    std.testing.expectEqual(
        @as(i18, InstructionABx.max_sbx),
        InstructionABx.unsignedBxToSigned(max_sbx_as_bx),
    );
}
