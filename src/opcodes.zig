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
            .call, .@"return" => .iABC,
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

pub const Instruction = packed union {
    raw: u32,
    iABC: packed struct {
        op: OpCode,
        A: u8,
        C: u9,
        B: u9,
    },
    iABx: packed struct {
        op: OpCode,
        A: u8,
        Bx: u18,
    },
    // TODO: how should sBx be handled? It seems like Lua treats it
    // as unsigned but then substracts int max from it to get the signed
    // version, so it might be better to just have a helper function
    // to convert between sBx <-> Bx when necessary
    // iAsBx: packed struct {
    //     op: OpCode,
    //     A: u8,
    //     sBx: u18,
    // },
};
