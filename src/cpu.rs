pub struct CpuEmu {
    A: u8,
    Y: u8,
    X: u8,
    PC: u16,
    S: u8,
    pub Flags: u8,
    pub Mem: [u8; 0xFFFF+1],
    pub clocks: u8,
}
const CFLAG: u8 = 0b00000001; //0b1111111a0
const ZFLAG: u8 = 0b00000010;//0b111110a1
pub const IFLAG: u8 = 0b00000100;
const DFLAG: u8 = 0b00001000;
const BFLAG: u8 = 0b00010000;
const VFLAG: u8 = 0b01000000;
const NFLAG: u8 = 0b10000000;

#[derive(Debug)]
struct Instr {
    cycles: u8,
    size: u8,
    inst_type: Instruction,
    mode: AddrMode,
}
#[derive(Debug, Clone)]
enum AddrMode {
    Accum, //Accumulator
    Imm, //Immediate
    Abs, //Absolute
    ZPg, //Zeropage
    ZPgX, //Zero Page X (Indexed Zero Page)
    ZPgY, //Zero Page Y (Indexed Zero Page)
    AbsX, //Absolute X
    AbsY, //Absolute Y
    Imp, //Implied
    Rel, //Relative
    IndX, //Indexed Indirect / Indirect X
    IndY, //Indirect Indexed / Indirect Y
    AbsInd, //Absolute Indirect
}
#[derive(Debug)]
enum Instruction {
    ADC,
    SBC,
    STA,
    STX,
    STY,
    LDA,
    LDX,
    LDY,
    BEQ,
    BRK,
    NOP,
    JMP,
    JSR,
    RTS,
    BMI,
    BNE,
    BPL,
    BVC,
    BVS,
    BCC,
    BCS,
    CLC,
    CLD,
    CLI,
    CLV,
    SEC,
    SED,
    SEI,
    CMP,
    CPY,
    CPX,
    DEC,
    DEX,
    DEY,
    INC,
    INX,
    INY,
    PHA,
    PHP,
    PLA,
    PLP,
    TAX,
    TAY,
    TSX,
    TXA,
    TXS,
    TYA,
    ORA,
    EOR,
    AND,
    ASL,
    ROL,
    LSR,
    BIT,
    RTI,
}
pub fn dec_to_hex(dec: u8) -> u8{
    let lo = (dec&0x0F)%0x0a;
    let mut c = 0;
    if (dec&0x0F) >= 0x0a {
        c = 1;
    }
    let hi = (((dec>>4)&0x0F)%0x0a+c);
    return (lo + hi*10)%100;
}
pub fn hex_to_dec(hex: u8) -> u8{
    let lo = hex%10;
    let hi = hex/10%10;
    return (lo + (hi*16)) as u8;
}
pub fn new(memory: &[u8], size: usize) -> CpuEmu{
    let mut emu = CpuEmu {
        A: 0,
        Y: 0,
        X: 0,
        PC: 0,
        S: 0x00FF,
        Flags: 0,
        Mem: [255; 0xFFFF+1],
        clocks: 0,
    };
    for i in 0..size {
        emu.Mem[i] = memory[i];
    }
    emu
}
impl CpuEmu{
    fn lookup(&mut self, instr: u8) -> Option<Instr>{
        match instr{
            0x69 => Some(Instr{cycles: 2, size: 2, inst_type: Instruction::ADC, mode: AddrMode::Imm}),
            0x6d => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::ADC, mode: AddrMode::Abs}),
            0x65 => Some(Instr{cycles: 3, size: 2, inst_type: Instruction::ADC, mode: AddrMode::ZPg}),
            0x61 => Some(Instr{cycles: 6, size: 2, inst_type: Instruction::ADC, mode: AddrMode::IndX}),
            0x71 => Some(Instr{cycles: 5, size: 2, inst_type: Instruction::ADC, mode: AddrMode::IndY}),
            0x75 => Some(Instr{cycles: 4, size: 2, inst_type: Instruction::ADC, mode: AddrMode::ZPgX}),
            0x7D => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::ADC, mode: AddrMode::AbsX}),
            0x79 => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::ADC, mode: AddrMode::AbsY}),

            0xe9 => Some(Instr{cycles: 2, size: 2, inst_type: Instruction::SBC, mode: AddrMode::Imm}),
            0xed => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::SBC, mode: AddrMode::Abs}),
            0xe5 => Some(Instr{cycles: 3, size: 2, inst_type: Instruction::SBC, mode: AddrMode::ZPg}),
            0xe1 => Some(Instr{cycles: 6, size: 2, inst_type: Instruction::SBC, mode: AddrMode::IndX}),
            0xf1 => Some(Instr{cycles: 5, size: 2, inst_type: Instruction::SBC, mode: AddrMode::IndY}),
            0xf5 => Some(Instr{cycles: 4, size: 2, inst_type: Instruction::SBC, mode: AddrMode::ZPgX}),
            0xfD => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::SBC, mode: AddrMode::AbsX}),
            0xf9 => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::SBC, mode: AddrMode::AbsY}),

            0xa9 => Some(Instr{cycles: 2, size: 2, inst_type: Instruction::LDA, mode: AddrMode::Imm}),
            0xad => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::LDA, mode: AddrMode::Abs}),
            0xa5 => Some(Instr{cycles: 3, size: 2, inst_type: Instruction::LDA, mode: AddrMode::ZPg}),
            0xa1 => Some(Instr{cycles: 6, size: 2, inst_type: Instruction::LDA, mode: AddrMode::IndX}),
            0xb1 => Some(Instr{cycles: 5, size: 2, inst_type: Instruction::LDA, mode: AddrMode::IndY}),
            0xb5 => Some(Instr{cycles: 4, size: 2, inst_type: Instruction::LDA, mode: AddrMode::ZPgX}),
            0xbd => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::LDA, mode: AddrMode::AbsX}),
            0xb9 => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::LDA, mode: AddrMode::AbsY}),

            0x8d => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::STA, mode: AddrMode::Abs}),
            0x85 => Some(Instr{cycles: 3, size: 2, inst_type: Instruction::STA, mode: AddrMode::ZPg}),
            0x81 => Some(Instr{cycles: 6, size: 2, inst_type: Instruction::STA, mode: AddrMode::IndX}),
            0x91 => Some(Instr{cycles: 6, size: 2, inst_type: Instruction::STA, mode: AddrMode::IndY}),
            0x95 => Some(Instr{cycles: 4, size: 2, inst_type: Instruction::STA, mode: AddrMode::ZPgX}),
            0x9D => Some(Instr{cycles: 5, size: 3, inst_type: Instruction::STA, mode: AddrMode::AbsX}),
            0x99 => Some(Instr{cycles: 5, size: 3, inst_type: Instruction::STA, mode: AddrMode::AbsY}),

            0xa2 => Some(Instr{cycles: 2, size: 2, inst_type: Instruction::LDX, mode: AddrMode::Imm}),
            0xae => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::LDX, mode: AddrMode::Abs}),
            0xa6 => Some(Instr{cycles: 3, size: 2, inst_type: Instruction::LDX, mode: AddrMode::ZPg}),
            0xbe => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::LDX, mode: AddrMode::AbsY}),
            0xb6 => Some(Instr{cycles: 4, size: 2, inst_type: Instruction::LDX, mode: AddrMode::ZPgY}),

            0xa0 => Some(Instr{cycles: 2, size: 2, inst_type: Instruction::LDY, mode: AddrMode::Imm}),
            0xac => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::LDY, mode: AddrMode::Abs}),
            0xa4 => Some(Instr{cycles: 3, size: 2, inst_type: Instruction::LDY, mode: AddrMode::ZPg}),
            0xb4 => Some(Instr{cycles: 4, size: 2, inst_type: Instruction::LDY, mode: AddrMode::ZPgX}),
            0xbc => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::LDY, mode: AddrMode::AbsX}),

            0x00 => Some(Instr{cycles: 7, size: 0, inst_type: Instruction::BRK, mode: AddrMode::Imp}),

            0xea => Some(Instr{cycles: 2, size: 1, inst_type: Instruction::NOP, mode: AddrMode::Imp}),

            0x8e => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::STX, mode: AddrMode::Abs}),
            0x86 => Some(Instr{cycles: 3, size: 2, inst_type: Instruction::STX, mode: AddrMode::ZPg}),
            0x96 => Some(Instr{cycles: 4, size: 2, inst_type: Instruction::STX, mode: AddrMode::ZPgY}),

            0x8c => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::STY, mode: AddrMode::Abs}),
            0x84=> Some(Instr{cycles: 3, size: 2, inst_type: Instruction::STY, mode: AddrMode::ZPgY}),
            0x94 => Some(Instr{cycles: 3, size: 2, inst_type: Instruction::STY, mode: AddrMode::ZPgY}),

            0x4c => Some(Instr{cycles: 3, size: 0, inst_type: Instruction::JMP, mode: AddrMode::Abs}),
            0x6c => Some(Instr{cycles: 5, size: 0, inst_type: Instruction::JMP, mode: AddrMode::AbsInd}),

            0x20 => Some(Instr{cycles: 6, size: 0, inst_type: Instruction::JSR, mode: AddrMode::Abs}),

            0x60 => Some(Instr{cycles: 6, size: 0, inst_type: Instruction::RTS, mode: AddrMode::Imp}),

            0xf0 => Some(Instr{cycles: 2, size: 2, inst_type: Instruction::BEQ, mode: AddrMode::Rel}),
            0x90 => Some(Instr{cycles: 2, size: 2, inst_type: Instruction::BCC, mode: AddrMode::Rel}),
            0xb0 => Some(Instr{cycles: 2, size: 2, inst_type: Instruction::BCS, mode: AddrMode::Rel}),
            0x30 => Some(Instr{cycles: 2, size: 2, inst_type: Instruction::BMI, mode: AddrMode::Rel}),
            0xd0 => Some(Instr{cycles: 2, size: 2, inst_type: Instruction::BNE, mode: AddrMode::Rel}),
            0x10 => Some(Instr{cycles: 2, size: 2, inst_type: Instruction::BPL, mode: AddrMode::Rel}),
            0x50 => Some(Instr{cycles: 2, size: 2, inst_type: Instruction::BVC, mode: AddrMode::Rel}),
            0x70 => Some(Instr{cycles: 2, size: 2, inst_type: Instruction::BVS, mode: AddrMode::Rel}),

            0x18 => Some(Instr{cycles: 2, size: 1, inst_type: Instruction::CLC, mode: AddrMode::Imp}),
            0xd8 => Some(Instr{cycles: 2, size: 1, inst_type: Instruction::CLD, mode: AddrMode::Imp}),
            0x58 => Some(Instr{cycles: 2, size: 1, inst_type: Instruction::CLI, mode: AddrMode::Imp}),
            0xb8 => Some(Instr{cycles: 2, size: 1, inst_type: Instruction::CLV, mode: AddrMode::Imp}),

            0x38 => Some(Instr{cycles: 2, size: 1, inst_type: Instruction::SEC, mode: AddrMode::Imp}),
            0xf8 => Some(Instr{cycles: 2, size: 1, inst_type: Instruction::SED, mode: AddrMode::Imp}),
            0x78 => Some(Instr{cycles: 2, size: 1, inst_type: Instruction::SEI, mode: AddrMode::Imp}),

            0xc9 => Some(Instr{cycles: 2, size: 2, inst_type: Instruction::CMP, mode: AddrMode::Imm}),
            0xcd => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::CMP, mode: AddrMode::Abs}),
            0xc5 => Some(Instr{cycles: 3, size: 2, inst_type: Instruction::CMP, mode: AddrMode::ZPg}),
            0xd5 => Some(Instr{cycles: 4, size: 2, inst_type: Instruction::CMP, mode: AddrMode::ZPgX}),
            0xc5 => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::CMP, mode: AddrMode::ZPg}),
            0xdd => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::CMP, mode: AddrMode::AbsX}),
            0xd9 => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::CMP, mode: AddrMode::AbsY}),
            0xc1 => Some(Instr{cycles: 6, size: 2, inst_type: Instruction::CMP, mode: AddrMode::IndX}),
            0xd1 => Some(Instr{cycles: 5, size: 2, inst_type: Instruction::CMP, mode: AddrMode::IndY}),

            0xe0 => Some(Instr{cycles: 2, size: 2, inst_type: Instruction::CPX, mode: AddrMode::Imm}),
            0xec => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::CPX, mode: AddrMode::Abs}),
            0xe4 => Some(Instr{cycles: 3, size: 2, inst_type: Instruction::CPX, mode: AddrMode::ZPg}),

            0xc0 => Some(Instr{cycles: 2, size: 2, inst_type: Instruction::CPY, mode: AddrMode::Imm}),
            0xcc => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::CPY, mode: AddrMode::Abs}),
            0xc4 => Some(Instr{cycles: 3, size: 2, inst_type: Instruction::CPY, mode: AddrMode::ZPg}),

            0xca => Some(Instr{cycles: 2, size: 1, inst_type: Instruction::DEX, mode: AddrMode::Imp}),
            0x88 => Some(Instr{cycles: 2, size: 1, inst_type: Instruction::DEY, mode: AddrMode::Imp}),

            0xce => Some(Instr{cycles: 6, size: 3, inst_type: Instruction::DEC, mode: AddrMode::Abs}),
            0xc6 => Some(Instr{cycles: 5, size: 2, inst_type: Instruction::DEC, mode: AddrMode::ZPg}),
            0xd6 => Some(Instr{cycles: 6, size: 2, inst_type: Instruction::DEC, mode: AddrMode::ZPgX}),
            0xde => Some(Instr{cycles: 7, size: 3, inst_type: Instruction::DEC, mode: AddrMode::AbsX}),

            0xe8 => Some(Instr{cycles: 2, size: 1, inst_type: Instruction::INX, mode: AddrMode::Imp}),
            0xc8 => Some(Instr{cycles: 2, size: 1, inst_type: Instruction::INY, mode: AddrMode::Imp}),

            0xee => Some(Instr{cycles: 6, size: 3, inst_type: Instruction::INC, mode: AddrMode::Abs}),
            0xe6 => Some(Instr{cycles: 5, size: 2, inst_type: Instruction::INC, mode: AddrMode::ZPg}),
            0xf6 => Some(Instr{cycles: 6, size: 2, inst_type: Instruction::INC, mode: AddrMode::ZPgX}),
            0xfe => Some(Instr{cycles: 7, size: 3, inst_type: Instruction::INC, mode: AddrMode::AbsX}),

            0x48 => Some(Instr{cycles: 3, size: 1, inst_type: Instruction::PHA, mode: AddrMode::Imp}),
            0x08 => Some(Instr{cycles: 3, size: 1, inst_type: Instruction::PHP, mode: AddrMode::Imp}),
            0x68 => Some(Instr{cycles: 4, size: 1, inst_type: Instruction::PLA, mode: AddrMode::Imp}),
            0x28 => Some(Instr{cycles: 4, size: 1, inst_type: Instruction::PLP, mode: AddrMode::Imp}),

            0xaa => Some(Instr{cycles: 2, size: 1, inst_type: Instruction::TAX, mode: AddrMode::Imp}),
            0xa8 => Some(Instr{cycles: 2, size: 1, inst_type: Instruction::TAY, mode: AddrMode::Imp}),
            0xba => Some(Instr{cycles: 2, size: 1, inst_type: Instruction::TSX, mode: AddrMode::Imp}),
            0x8a => Some(Instr{cycles: 2, size: 1, inst_type: Instruction::TXA, mode: AddrMode::Imp}),
            0x9a => Some(Instr{cycles: 2, size: 1, inst_type: Instruction::TXS, mode: AddrMode::Imp}),
            0x98 => Some(Instr{cycles: 2, size: 1, inst_type: Instruction::TYA, mode: AddrMode::Imp}),

            0x29 => Some(Instr{cycles: 2, size: 2, inst_type: Instruction::AND, mode: AddrMode::Imm}),
            0x2d => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::AND, mode: AddrMode::Abs}),
            0x25 => Some(Instr{cycles: 3, size: 2, inst_type: Instruction::AND, mode: AddrMode::ZPg}),
            0x21 => Some(Instr{cycles: 6, size: 2, inst_type: Instruction::AND, mode: AddrMode::IndX}),
            0x31 => Some(Instr{cycles: 5, size: 2, inst_type: Instruction::AND, mode: AddrMode::IndY}),
            0x35 => Some(Instr{cycles: 4, size: 2, inst_type: Instruction::AND, mode: AddrMode::ZPgX}),
            0x3D => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::AND, mode: AddrMode::AbsX}),
            0x39 => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::AND, mode: AddrMode::AbsY}),

            0x49 => Some(Instr{cycles: 2, size: 2, inst_type: Instruction::EOR, mode: AddrMode::Imm}),
            0x4d => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::EOR, mode: AddrMode::Abs}),
            0x45 => Some(Instr{cycles: 3, size: 2, inst_type: Instruction::EOR, mode: AddrMode::ZPg}),
            0x41 => Some(Instr{cycles: 6, size: 2, inst_type: Instruction::EOR, mode: AddrMode::IndX}),
            0x51 => Some(Instr{cycles: 5, size: 2, inst_type: Instruction::EOR, mode: AddrMode::IndY}),
            0x55 => Some(Instr{cycles: 4, size: 2, inst_type: Instruction::EOR, mode: AddrMode::ZPgX}),
            0x5D => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::EOR, mode: AddrMode::AbsX}),
            0x59 => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::EOR, mode: AddrMode::AbsY}),

            0x09 => Some(Instr{cycles: 2, size: 2, inst_type: Instruction::ORA, mode: AddrMode::Imm}),
            0x0d => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::ORA, mode: AddrMode::Abs}),
            0x05 => Some(Instr{cycles: 3, size: 2, inst_type: Instruction::ORA, mode: AddrMode::ZPg}),
            0x01 => Some(Instr{cycles: 6, size: 2, inst_type: Instruction::ORA, mode: AddrMode::IndX}),
            0x11 => Some(Instr{cycles: 5, size: 2, inst_type: Instruction::ORA, mode: AddrMode::IndY}),
            0x15 => Some(Instr{cycles: 4, size: 2, inst_type: Instruction::ORA, mode: AddrMode::ZPgX}),
            0x1D => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::ORA, mode: AddrMode::AbsX}),
            0x19 => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::ORA, mode: AddrMode::AbsY}),

            0x0e => Some(Instr{cycles: 6, size: 3, inst_type: Instruction::ASL, mode: AddrMode::Abs}),
            0x0a => Some(Instr{cycles: 2, size: 1, inst_type: Instruction::ASL, mode: AddrMode::Accum}),
            0x06 => Some(Instr{cycles: 5, size: 2, inst_type: Instruction::ASL, mode: AddrMode::ZPg}),
            0x16 => Some(Instr{cycles: 6, size: 2, inst_type: Instruction::ASL, mode: AddrMode::ZPgX}),
            0x1e => Some(Instr{cycles: 7, size: 3, inst_type: Instruction::ASL, mode: AddrMode::AbsX}),

            0x2e => Some(Instr{cycles: 6, size: 3, inst_type: Instruction::ROL, mode: AddrMode::Abs}),
            0x2a => Some(Instr{cycles: 2, size: 1, inst_type: Instruction::ROL, mode: AddrMode::Accum}),
            0x26 => Some(Instr{cycles: 5, size: 2, inst_type: Instruction::ROL, mode: AddrMode::ZPg}),
            0x36 => Some(Instr{cycles: 6, size: 2, inst_type: Instruction::ROL, mode: AddrMode::ZPgX}),
            0x3e => Some(Instr{cycles: 7, size: 3, inst_type: Instruction::ROL, mode: AddrMode::AbsX}),

            0x4e => Some(Instr{cycles: 6, size: 3, inst_type: Instruction::LSR, mode: AddrMode::Abs}),
            0x4a => Some(Instr{cycles: 2, size: 1, inst_type: Instruction::LSR, mode: AddrMode::Accum}),
            0x46 => Some(Instr{cycles: 5, size: 2, inst_type: Instruction::LSR, mode: AddrMode::ZPg}),
            0x56 => Some(Instr{cycles: 6, size: 2, inst_type: Instruction::LSR, mode: AddrMode::ZPgX}),
            0x5e => Some(Instr{cycles: 7, size: 3, inst_type: Instruction::LSR, mode: AddrMode::AbsX}),

            0x2c => Some(Instr{cycles: 4, size: 3, inst_type: Instruction::BIT, mode: AddrMode::Abs}),
            0x24 => Some(Instr{cycles: 3, size: 2, inst_type: Instruction::BIT, mode: AddrMode::ZPg}),

            0x40 => Some(Instr{cycles: 6, size: 0, inst_type: Instruction::RTI, mode: AddrMode::Imp}),

            _ => None,
        }
    }
    fn run_instr(&mut self, instr: Instruction, mode: AddrMode, op1: u8, op2: u8, DEBUG: bool){
        let (address, clock_penalty) = self.fetch(op1, op2, mode.clone());
        if DEBUG == true {
            println!("accessing address: {:#06x} with mode {:?}",address, mode);
        }
        match instr {
            Instruction::NOP => {},
            Instruction::ADC => {
                self.clocks += clock_penalty;
                if self.Flags & DFLAG != 0 {
                    let adding = dec_to_hex(self.Mem[address as usize]);
                    let tmpA = dec_to_hex(self.A);
                    self.A = hex_to_dec(tmpA.wrapping_add(adding)+(self.Flags&CFLAG));


                    if tmpA > 99 - adding {self.Flags = self.Flags | CFLAG;
                    } else {
                        self.Flags = self.Flags & !CFLAG;}
                    self.set_flagsnz(self.A);
                } else {
                    let adding = self.Mem[address as usize];
                    let tmpA = self.A;
                    self.A = self.A.wrapping_add(adding)+(self.Flags&CFLAG);
                    if tmpA > 255 - adding {self.Flags = self.Flags | CFLAG;
                    } else {
                        self.Flags = self.Flags & !CFLAG;}
                    self.set_flagsnz(self.A);
                    self.set_overflow(tmpA, adding);
                }
            },
            Instruction::SBC => {
                self.clocks += clock_penalty;
                if self.Flags & DFLAG != 0 {
                    let subtracting = !dec_to_hex(self.Mem[address as usize]);
                    let tmpA = dec_to_hex(self.A);
                    let subby: u8 = subtracting.wrapping_sub(!(self.Flags&CFLAG));
                    self.A = hex_to_dec(tmpA.wrapping_add(subby));

                    if tmpA > 99_u8.wrapping_sub(subby) {self.Flags = self.Flags & !CFLAG;
                    } else {
                        self.Flags = self.Flags | CFLAG;}
                    self.set_flagsnz(self.A);
                } else {
                    let subtracting = !(self.Mem[address as usize]);
                    let tmpA = self.A;
                    self.A = self.A.wrapping_add(subtracting.wrapping_sub(!(self.Flags&CFLAG)));
                    if tmpA > 255 - subtracting.wrapping_sub(!(self.Flags&CFLAG)) {self.Flags = self.Flags & !CFLAG;
                    } else {
                        self.Flags = self.Flags | CFLAG;}
                    self.set_flagsnz(self.A);
                    self.set_overflow(tmpA, subtracting);
                }
            },
            Instruction::CMP => {
                self.clocks += clock_penalty;
                let subtracting = (self.Mem[address as usize]);
                let res = self.A.wrapping_add(!(subtracting)+1);
                if self.A >= subtracting {self.Flags = self.Flags | CFLAG;
                } else {
                    self.Flags = self.Flags & !CFLAG;}
                self.set_flagsnz(res);
            },
            Instruction::CPX => {
                let subtracting = (self.Mem[address as usize]);
                let res = self.X.wrapping_add(!(subtracting)+1);
                if self.X >= subtracting {self.Flags = self.Flags | CFLAG;
                } else {
                    self.Flags = self.Flags & !CFLAG;}
                self.set_flagsnz(res);
            },
            Instruction::CPY => {
                let subtracting = (self.Mem[address as usize]);
                let res = self.Y.wrapping_add(!(subtracting)+1);
                if self.Y >= subtracting {self.Flags = self.Flags | CFLAG;
                } else {
                    self.Flags = self.Flags & !CFLAG;}
                self.set_flagsnz(res);
            },
            Instruction::AND => {
                self.clocks += clock_penalty;
                self.A = self.A & self.Mem[address as usize];
                self.set_flagsnz(self.A);
            },
            Instruction::BIT => {
                let mem =self.Mem[address as usize];
                let res = self.A & mem;
                self.Flags = (mem & NFLAG) & !NFLAG;
                self.Flags = (mem & VFLAG) & !VFLAG;
                if res == 0 {self.Flags = self.Flags | ZFLAG;}
            },
            Instruction::EOR => {
                self.clocks += clock_penalty;
                self.A = self.A ^ self.Mem[address as usize];
                self.set_flagsnz(self.A);
            },
            Instruction::ORA => {
                self.clocks += clock_penalty;
                self.A = self.A | self.Mem[address as usize];
                self.set_flagsnz(self.A);
            },
            Instruction::LDA => {
                self.clocks += clock_penalty;
                self.A = self.Mem[address as usize];
                self.set_flagsnz(self.A);
            },
            Instruction::STA => {
                self.Mem[address as usize] = self.A;
            },
            Instruction::STX => {
                self.Mem[address as usize] = self.X;
            },
            Instruction::STY => {
                self.Mem[address as usize] = self.Y;
            },
            Instruction::LDX => {
                self.clocks += clock_penalty;
                self.X = self.Mem[address as usize];
                self.set_flagsnz(self.X);
            },
            Instruction::LDY => {
                self.clocks += clock_penalty;
                self.Y = self.Mem[address as usize];
                self.set_flagsnz(self.Y);
            },
            Instruction::ASL => {
                self.Flags = self.Flags & !CFLAG;
                if let AddrMode::Accum = mode {
                    self.Flags = self.Flags | ((self.A & NFLAG) >> 7);
                    self.A = self.A << 1;
                    self.set_flagsnz(self.A);
                } else {
                    let mem = self.Mem[address as usize];
                    self.Flags = self.Flags | ((mem & NFLAG) >> 7);
                    self.Mem[address as usize] = mem << 1;
                    self.set_flagsnz(self.Mem[address as usize]);
                }
            },
            Instruction::LSR => {
                self.Flags = self.Flags & !CFLAG;
                if let AddrMode::Accum = mode{
                    self.Flags = self.Flags | (self.A & CFLAG);
                    self.A = self.A >> 1;
                    self.set_flagsnz(self.A);
                } else {
                    let mem = self.Mem[address as usize];
                    self.Flags = self.Flags | (mem & CFLAG);
                    self.Mem[address as usize] = mem >> 1;
                    self.set_flagsnz(self.Mem[address as usize]);
                }
            },
            Instruction::ROL => {
                self.Flags = self.Flags & !CFLAG;
                if let AddrMode::Accum = mode {
                    self.Flags = self.Flags | ((self.A & NFLAG) >> 7);
                    self.A = self.A << 1;
                    self.A += self.Flags & CFLAG;
                    self.set_flagsnz(self.A);
                } else {
                    let mem = self.Mem[address as usize];
                    self.Flags = self.Flags | ((mem & NFLAG) >> 7);
                    self.Mem[address as usize] = mem << 1;
                    self.Mem[address as usize] += self.Flags & CFLAG;
                    self.set_flagsnz(self.Mem[address as usize]);
                }

            }
            Instruction::DEC => {
                self.Mem[address as usize] -= 1;
                self.set_flagsnz(self.Mem[address as usize]);
            },
            Instruction::DEX => {
                self.X -= 1;
                self.set_flagsnz(self.X);
            },
            Instruction::DEY => {
                self.Y -= 1;
                self.set_flagsnz(self.Y);
            },
            Instruction::INC => {
                self.Mem[address as usize] += 1;
                self.set_flagsnz(self.Mem[address as usize]);
            },
            Instruction::INX => {
                self.X += 1;
                self.set_flagsnz(self.X);
            },
            Instruction::INY => {
                self.Y += 1;
                self.set_flagsnz(self.Y);
            },
            Instruction::PHA => {
                self.Mem[self.S as usize] = self.A;
                self.S -= 1;
            },
            Instruction::PHP => {
                self.Mem[self.S as usize] = self.Flags;
                self.S -= 1;
            },
            Instruction::PLP => {
                self.S += 1;
                self.Flags = self.Mem[self.S as usize];
            },
            Instruction::PLA => {
                self.S += 1;
                self.A = self.Mem[self.S as usize];
            },
            Instruction::TAX => {
                self.X = self.A;
                self.set_flagsnz(self.X);
            },
            Instruction::TAY => {
                self.Y = self.A;
                self.set_flagsnz(self.Y);
            },
            Instruction::TSX => {
                self.X = self.S;
                self.set_flagsnz(self.X);
            },
            Instruction::TXA => {
                self.A = self.X;
                self.set_flagsnz(self.A);
            },
            Instruction::TXS => {
                self.S = self.X;
            },
            Instruction::TYA => {
                self.A = self.Y;
                self.set_flagsnz(self.A);
            },
            Instruction::JMP => {
                self.PC = address;
            },
            Instruction::JSR => {
                let pc_add: u16 = self.PC.wrapping_add(2);
                self.Mem[self.S as usize] = (pc_add&0x00FF) as u8;
                self.S -= 1;
                self.Mem[self.S as usize] = ((pc_add&0xFF00) >> 8) as u8;
                self.S -= 1;
                self.PC = address;
            },
            Instruction::RTI => {
                self.S += 1;
                self.Flags = self.Mem[self.S as usize];
                let mut pctmp: u16 = 0;
                self.S += 1;
                pctmp = pctmp | (self.Mem[self.S as usize] as u16) << 8;
                self.S += 1;
                pctmp = pctmp | (self.Mem[self.S as usize] as u16);
                self.PC = pctmp;
            },
            Instruction::RTS => {
                let mut pctmp: u16 = 0;
                self.S += 1;
                pctmp = pctmp | (self.Mem[self.S as usize] as u16) << 8;
                self.S += 1;
                pctmp = pctmp | (self.Mem[self.S as usize] as u16);
                self.PC = pctmp + 1;
            },
            Instruction::BMI => {
                self.clocks += clock_penalty;
                self.branch_flag_set(0b10000000, address);
            },
            Instruction::BCS => {
                self.clocks += clock_penalty;
                self.branch_flag_set(CFLAG, address);
            },
            Instruction::BVS => {
                self.clocks += clock_penalty;
                self.branch_flag_set(VFLAG, address);
            },
            Instruction::BEQ => {
                self.clocks += clock_penalty;
                self.branch_flag_set(ZFLAG, address);
            },
            Instruction::BPL => {
                self.clocks += clock_penalty;
                self.branch_flag_not_set(0b10000000, address);
            },
            Instruction::BCC => {
                self.clocks += clock_penalty;
                self.branch_flag_not_set(CFLAG, address);
            },
            Instruction::BVC => {
                self.clocks += clock_penalty;
                self.branch_flag_not_set(VFLAG, address);
            },
            Instruction::BNE => {
                self.clocks += clock_penalty;
                self.branch_flag_not_set(ZFLAG, address);
            },
            Instruction::CLC => {
                self.Flags = self.Flags & !CFLAG
            },
            Instruction::CLD => {
                self.Flags = self.Flags & !DFLAG
            },
            Instruction::CLI => {
                self.Flags = self.Flags & !IFLAG
            },
            Instruction::CLV => {
                self.Flags = self.Flags | VFLAG
            },
            Instruction::SEC => {
                self.Flags = self.Flags | CFLAG
            },
            Instruction::SED => {
                self.Flags = self.Flags | DFLAG
            },
            Instruction::SEI => {
                self.Flags = self.Flags | IFLAG
            },
            Instruction::BRK => {
                self.interrupt(BFLAG, 0xFFFE);
            },
            _ => {},
        }
    }
    pub fn interrupt(&mut self, b_flag: u8, interrupt_base_addr: u16) {
        self.PC.wrapping_add(1);
        self.Flags = self.Flags & !BFLAG;
        self.Flags = self.Flags | IFLAG;
        self.Mem[self.S as usize] = ((self.PC & 0xFF00) >> 8) as u8;
        self.S -= 1;
        self.Mem[self.S as usize] = ((self.PC & 0x00FF)) as u8;
        self.S -= 1;
        self.Mem[self.S as usize] = self.Flags | b_flag;
        self.PC = u16::from_le_bytes([self.Mem[interrupt_base_addr as usize], self.Mem[interrupt_base_addr as usize+1]]);
    }
    fn branch_flag_not_set(&mut self, flag: u8, jump_addr: u16){
        if self.Flags & flag == 0 {
            self.clocks += 1;
            if self.PC & 0xFF00 != jump_addr & 0xFF00 {
                self.clocks += 1;
            }
            self.PC = jump_addr;
        }
    }
    fn branch_flag_set(&mut self, flag: u8, jump_addr: u16){
        if self.Flags & flag != 0{
            self.clocks += 1;
            if self.PC & 0xFF00 != jump_addr & 0xFF00 {
                self.clocks += 1;
            }
            self.PC = jump_addr;
        }
    }
    fn set_overflow(&mut self, M: u8, N: u8){
        //sets overflow flag
        if ((M^self.A)&(N^self.A))&0b10000000 != 0{self.Flags = self.Flags | VFLAG
        } else {
            self.Flags = self.Flags & !VFLAG;}
    }
    fn set_flagsnz(&mut self, M: u8){
        //sets Negative flag
        if M & 0b10000000 != 0 {self.Flags = self.Flags | NFLAG}
        else {
            self.Flags = self.Flags & !NFLAG;}

        //sets zero flag
        if M == 0 {self.Flags = self.Flags | ZFLAG;
        } else {
            self.Flags = self.Flags & !ZFLAG;}
    }
    pub fn clock(&mut self, DEBUG: bool) -> Result<(), String>{
        if self.clocks != 0{
            self.clocks -= 1;
            return Ok(());
        } else {
            let instr_code = self.Mem[self.PC as usize];
            let instr = self.lookup(instr_code);
            if instr.is_none(){
                if instr_code == 255 {
                    return Err("reached code 255, end of assembly".to_owned());
                }else {return Err(format!("reached invalid code: {}", instr_code));}
            }
            let instr = instr.unwrap();
            if self.PC as u32 + instr.size as u32 > self.Mem.len() as u32 -1{
                return Err("attempted to access outside of memory".to_owned());
            }
            if DEBUG == true{
                println!("type: {:?}", &instr.inst_type);
            }
            self.run_instr(instr.inst_type, instr.mode, self.Mem[self.PC as usize+1], self.Mem[self.PC as usize +2], DEBUG);
            self.PC += instr.size as u16;

            self.clocks += instr.cycles-1; //minus the cycle to read the instruction

            if DEBUG == true{
                self.print_state();
            }
            return Ok(());


        }
    }
    pub fn print_state(&mut self){
        println!("A: {}\nA(signed): {}, \nA(Hex): {:#04x}\nX: {}\nY: {}\nPC: {:#06x}\nS: {}\nclocks left: {}\nFLAGS:\n----------\nNegative: {}\nOverflow: {}\nBRK: {}\nDecimal: {}\nIRQ Disable: {}\nZero: {}\nCarry: {}\n\n", self.A, self.A as i8, self.A, self.X, self.Y, self.PC, self.S, self.clocks, self.Flags & NFLAG,self.Flags & VFLAG,self.Flags & BFLAG,self.Flags & DFLAG,self.Flags & IFLAG, self.Flags & ZFLAG, self.Flags & CFLAG);
    }
    fn fetch(&mut self, op1: u8, op2: u8, mode: AddrMode) -> (u16, u8){

        let mut clock_penalty = 0;
        match mode {
            AddrMode::Imm => (self.PC + 1, 0),
            AddrMode::Abs => {
                return (u16::from_le_bytes([op1, op2]), 0);
            },
            AddrMode::ZPg => {
                return (op1 as u16, 0);
            },
            AddrMode::IndX => {
                let base = self.X.wrapping_add(op1);
                let low_order = self.Mem[base as usize];
                let high_order = self.Mem[base as usize +1];
                return (u16::from_le_bytes([low_order, high_order]), 0);
            },
            AddrMode::IndY => {
                let low_order = self.Mem[op1 as usize].wrapping_add(self.Y);
                let mut carry = 0;
                if self.Mem[op1 as usize] > 255 - self.Y {
                    carry = 1;
                    clock_penalty +=1; //if the page boundary is crossed, extra clock because if there is a carry it must add it to the high byte, leading to an extra cycle.
                }
                let high_order = self.Mem[op1 as usize+1]+carry;
                return (u16::from_le_bytes([low_order, high_order]), clock_penalty);
            },
            AddrMode::ZPgX => {
                return (self.X.wrapping_add(op1) as u16, 0);
            },
            AddrMode::ZPgY => {
                return (self.Y.wrapping_add(op1) as u16, 0);
            },
            AddrMode::AbsX=> {
                let low_order = op1.wrapping_add(self.X);
                let mut carry = 0;
                if op1 > 255 - self.X {
                    carry = 1;
                    clock_penalty +=1; //if the page boundary is crossed, extra clock because if there is a carry it must add it to the high byte, leading to an extra cycle.
                }
                let high_order = op2+carry;
                return (u16::from_le_bytes([low_order, high_order]), clock_penalty);
            },
            AddrMode::AbsY => {
                let base = u16::from_le_bytes([op1, op2]);
                let addr = base.wrapping_add(self.Y as u16);
                if base&0xFF00 != addr&0xFF00 {
                    clock_penalty +=1;
                }
                return (addr, clock_penalty);
            },
            AddrMode::Rel => {
                let mut pctmp = 0;
                if op1 & 0b10000000 != 0 {
                    pctmp = self.PC.wrapping_sub(op1 as u16);
                } else {
                    pctmp = self.PC.wrapping_add(op1 as u16);
                }
                if pctmp&0xFF00 != self.PC&0xFF00 {
                    clock_penalty += 1;
                }
                return (pctmp, clock_penalty);
            }
            AddrMode::AbsInd => {
                let ind_addr = u16::from_le_bytes([op1, op2]);
                return (u16::from_le_bytes([self.Mem[ind_addr as usize], self.Mem[ind_addr as usize +1]]),0);
            }
            _ => (0, 0),
        }
    }
}
