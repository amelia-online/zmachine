const std = @import("std");
const ln: u8 = 10;

//Registers

//No register                     0
//@ret => for returning values    1
//@spr => stack pointer           2
//@glb => global pointer          3
//@rta => return address          4
//@fla => flags                   5
//@fpr => frame pointer           6

//0000 0000 0000 0000
//^    ^    ^    ^
//CF   ZF   SF   OF

//@rg0 => general purpose         7
//@rg1                            8
//@rg2                            9
//@rg3                            10
//@rg4                            11

//@pr0 => param0                  12
//@pr1                            13
//@pr2                            14

//@rg5                            15

const Register = enum(u4) {
    ret = 0x1,
    spr = 0x2,
    glb = 0x3,
    rta = 0x4,
    fla = 0x5,
    fpr = 0x6,
    rg0 = 0x7,
    rg1 = 0x8,
    rg2 = 0x9,
    rg3 = 0xA,
    rg4 = 0xB,
    pr0 = 0xC,
    pr1 = 0xD,
    pr2 = 0xE,
    rg5 = 0xF,
};

const Exp = enum {
    writes_addr_imm, // 0
    writes_reg_imm,
    write,
    read,
    copy8_reg_reg,
    copy16_reg_reg,
    copy32_reg_reg,
    copy64_reg_reg,
    copy8_reg_imm, // 8
    copy16_reg_imm,
    copy32_reg_imm,
    copy64_reg_imm,
    copy8_reg_deref, // 12
    copy16_reg_deref,
    copy32_reg_deref,
    copy64_reg_deref,
    copy8_deref_reg, // 16
    copy16_deref_reg,
    copy32_deref_reg,
    copy64_deref_reg,
    push8,
    push16,
    push32,
    push64,
    pop8, // 24
    pop16,
    pop32,
    pop64,
    goto,
    call,
    sys,
    cmp_reg_reg,
    cmp_reg_imm, // 32
    jge,
    jg,
    jle,
    jl,
    je,
    jne,
    add_reg_reg,
    add_reg_deref, // 40
    add_reg_imm,
    sub_reg_reg,
    sub_reg_deref,
    sub_reg_imm,
    mult_reg_reg,
    mult_reg_deref,
    mult_reg_imm,
    div_reg_reg, // 48
    div_reg_deref,
    div_reg_imm,
    mod_reg_reg,
    mod_reg_deref,
    mod_reg_imm,
    shr_reg_imm,
    shl_reg_imm,
    and_reg_reg, // 56
    and_reg_imm,
    or_reg_reg,
    or_reg_imm,
    xor_reg_reg,
    xor_reg_imm,
    not_reg, // 62
    copy8_deref_imm, // 63
    write_int_reg,
};

const AsmError = error{
    IncorrectFormat,
    InvalidOpcode,
};

const Instr = struct {
    opcode: u8,
    reg1: u4,
    reg2: u4,
    immediate: u16,

    pub fn new(opcode: u8, reg1: u4, reg2: u4, imm: u16) Instr {
        return Instr{
            .opcode = opcode,
            .reg1 = reg1,
            .reg2 = reg2,
            .immediate = imm,
        };
    }

    pub fn from(instr: u32) Instr {
        return Instr{
            .opcode = @intCast((instr >> 24) & 0xFF),
            .reg1 = @intCast((instr >> 20) & 0xF),
            .reg2 = @intCast((instr >> 24) & 0xF),
            .immediate = @intCast(instr & 0xFFFF),
        };
    }

    pub fn toString(self: *const Instr, alloc: std.mem.Allocator) ![]const u8 {
        const res = try std.fmt.allocPrint(alloc, "({d} {d} {d} {d})", .{ self.opcode, self.reg1, self.reg2, self.immediate });
        return res;
    }
};

pub fn usage() !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("usage: ./zm <path>\n", .{});
    try stdout.print("       ./zm --asm <path>\n", .{});
}

pub fn readFileBytes(path: []const u8, allocator: std.mem.Allocator) ![]u8 {
    const bytes = try std.fs.cwd().readFileAlloc(allocator, path, 2000000);
    return bytes;
}

//0000 0000 0000 0000 0000 0000 0000 0000
//^-------- ^--- ^--- ^------------------
//opcode    r1   r2   immediate

pub fn loadInstructions(bytes: []u8, allocator: std.mem.Allocator) !std.ArrayList(Instr) {
    var instrs = std.ArrayList(Instr).init(allocator);
    errdefer instrs.deinit();
    errdefer allocator.free(bytes);

    if (bytes.len % 4 != 0) {
        return AsmError.IncorrectFormat;
    }

    var i: u64 = 0;
    while (i < bytes.len) : (i += 4) {
        const opcode: u8 = bytes[i];
        const r1: u4 = @intCast((bytes[i + 1] >> 4) & 0xF);
        const r2: u4 = @intCast((bytes[i + 1]) & 0xF);
        var imm: u16 = bytes[i + 2];
        imm <<= 8;
        imm |= bytes[i + 3];
        try instrs.append(Instr.new(opcode, r1, r2, imm));
    }

    return instrs;
}

pub fn run(instrs: std.ArrayList(Instr), memory: []u8, registers: []i32, alloc: std.mem.Allocator) !void {
    const stdout = std.io.getStdOut().writer();
    _ = alloc;
    for (instrs.items) |instr| {
        switch (instr.opcode) {
            0 => { // writes_addr_imm : writes <@reg :- addr> <imm :- len>
                const regval = registers[instr.reg1];
                const len = instr.immediate;
                const idx: usize = @intCast(regval);

                for (0..len) |i| {
                    // std.debug.print("here\n", .{});
                    try stdout.print("{c}", .{memory[idx + i]});
                }
            },

            6 => { //copy32_reg_reg : copy32 <@reg1>, <@reg2>
                const r1: usize = @intCast(instr.reg1);
                const r2: usize = @intCast(instr.reg2);
                registers[r1] = registers[r2];
            },

            // ...
            8 => { // copy8_reg_imm : copy8 <@reg>, <imm>
                const regval: usize = @intCast(instr.reg1);
                registers[regval] = instr.immediate;
            },

            // ...
            12 => { // copy8_deref_reg : copy8 [<@reg1>], <@reg2>
                const r1: usize = @intCast(instr.reg1);
                const r2: usize = @intCast(instr.reg2);
                const idx: usize = @intCast(registers[r1]);
                const value: u8 = @intCast(registers[r2] & 0xFF);
                memory[idx] = value;
            },

            14 => { // copy32_reg_deref : copy32 <@reg1>, [<@reg2>]
                const r1: usize = @intCast(instr.reg1);
                const idx: usize = @intCast(registers[instr.reg2]);
                const value = std.mem.readPackedIntNative(i32, memory, idx);
                registers[r1] = value;
            },

            18 => { // copy32_deref_reg : copy32 [<@reg1>], <@reg2>
                const addr: usize = @intCast(registers[instr.reg1]);
                const r2: usize = @intCast(instr.reg2);
                std.mem.writePackedIntNative(i32, memory, addr, registers[r2]);
            },

            20 => { // push8_reg : push8 <@reg>
                registers[0x2] -= 1;
                const idx: usize = @intCast(registers[0x2]); // @spr
                memory[idx] = @intCast(registers[instr.reg1] & 0xFF);
            },

            24 => { // pop8_reg : pop8 <@reg>
                const idx: usize = @intCast(registers[0x2]);
                registers[instr.reg1] = memory[idx];
                registers[0x2] += 1;
            },

            39 => { // add_reg_reg : add <@reg1>, <@reg2>
                registers[instr.reg1] +%= registers[instr.reg2];
            },

            40 => { // add_reg_deref : add <@reg1>, [<@reg2>]
                const idx: usize = @intCast(registers[instr.reg2]);
                registers[instr.reg1] +%= memory[idx];
            },

            41 => { // add_reg_imm : add <@reg>, <imm>
                const r1: usize = @intCast(instr.reg1);
                registers[r1] +%= instr.immediate;
            },

            42 => { // sub_reg_reg : sub <@reg1>, <@reg2>
                registers[instr.reg1] -%= registers[instr.reg2];
            },

            43 => { // sub_reg_deref : sub <@reg1>, [<@reg2>]
                const idx: usize = @intCast(registers[instr.reg2]);
                registers[instr.reg1] -%= memory[idx];
            },

            44 => { // sub_reg_imm : sub <@reg>, <imm>
                const r1: usize = @intCast(instr.reg1);
                registers[r1] -%= instr.immediate;
            },

            45 => { // mult_reg_reg : mult <@reg1>, <@reg2>
                registers[instr.reg1] *%= registers[instr.reg2];
            },

            46 => { // mult_reg_deref : mult <@reg1>, [<@reg2>]
                const idx: usize = @intCast(registers[instr.reg2]);
                registers[instr.reg1] *%= memory[idx];
            },

            47 => { // mult_reg_imm : mult <@reg>, <imm>
                const r1: usize = @intCast(instr.reg1);
                registers[r1] *%= instr.immediate;
            },

            48 => { // div_reg_reg : div <@reg1>, <@reg2>
                registers[instr.reg1] = @divFloor(registers[instr.reg1], registers[instr.reg2]);
            },

            49 => { // div_reg_deref : div <@reg1>, [<@reg2>]
                const idx: usize = @intCast(registers[instr.reg2]);
                registers[instr.reg1] = @divFloor(registers[instr.reg1], memory[idx]);
            },

            50 => { // div_reg_imm : div <@reg>, <imm>
                const r1: usize = @intCast(instr.reg1);
                registers[r1] = @divFloor(registers[r1], instr.immediate);
            },

            51 => { // mod_reg_reg : mod <@reg1>, <@reg2>
                registers[instr.reg1] = @mod(registers[instr.reg1], registers[instr.reg2]);
            },

            52 => { // mod_reg_deref : mod <@reg1>, [<@reg2>]
                const idx: usize = @intCast(registers[instr.reg2]);
                registers[instr.reg1] = @mod(registers[instr.reg1], memory[idx]);
            },

            53 => { // mod_reg_imm : mult <@reg>, <imm>
                const r1: usize = @intCast(instr.reg1);
                registers[r1] = @mod(registers[r1], instr.immediate);
            },

            // ...
            63 => { // copy8_deref_imm : copy8 [<@reg1>], <imm>
                const r1: usize = @intCast(instr.reg1);
                const addr: usize = @intCast(registers[r1]);
                const value: u8 = @truncate(instr.immediate);
                memory[addr] = value;
            },

            64 => { // write_int_reg : writei <@reg>
                const value = registers[instr.reg1];
                //std.debug.print("here", .{});
                try stdout.print("{d}", .{value});
            },

            else => {
                return AsmError.InvalidOpcode;
            },
        }
    }
}

pub fn main() !void {
    // const stdout = std.io.getStdOut().writer();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const bytes = try readFileBytes("./bytes", allocator);
    const instrs = try loadInstructions(bytes, allocator);
    defer instrs.deinit();
    allocator.free(bytes);

    //const args = try std.process.argsAlloc(allocator);
    //defer std.process.argsFree(allocator, args);

    // for (args) |arg| {
    //     std.debug.print("{s}\n", .{arg});
    // }

    const memory = try allocator.alloc(u8, 1024);
    defer allocator.free(memory);

    const registers = try allocator.alloc(i32, 15);
    defer allocator.free(registers);

    for (memory) |*byte| {
        byte.* = 0;
    }

    for (registers) |*reg| {
        reg.* = 0;
    }

    registers[0x2] = 1024; // @spr = 1024, top of stack

    try run(instrs, memory, registers, allocator);
}
