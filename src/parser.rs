use std::fmt::{self, Display};
use std::path::PathBuf;

use nom::bits::bits;
use nom::bits::complete::{tag, take};
use nom::branch::alt;
use nom::bytes::complete::{tag as tag_bytes, take as take_bytes};
use nom::combinator::{iterator, map, opt};
use nom::sequence::tuple;
use nom::IResult;

use crate::utils::print_vec_u8_bits;

pub enum RM {
    Reg(Register),
    Mem(MemAddr),
    Imd(Literal),
    SegReg(Segment),
}

impl Display for RM {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Reg(reg) => reg.fmt(f),
            Self::Mem(ea) => ea.fmt(f),
            Self::Imd(imd) => match imd {
                Literal::Byte(x) => write!(f, "byte {}", x),
                Literal::Word(x) => write!(f, "word {}", x),
            },
            Self::SegReg(sg) => sg.fmt(f),
        }
    }
}

pub enum UnaryDest {
    RM(RM),
    InterSeg((Literal, Literal)),
    Offset(Offset),
}

impl Display for UnaryDest {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::RM(rm) => rm.fmt(f),
            Self::InterSeg((start, end)) => write!(f, "{}:{}", start, end),
            Self::Offset(offset) => offset.fmt(f),
        }
    }
}

pub enum ShiftRotSrc {
    RM(RM),
    One,
}

impl Display for ShiftRotSrc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::RM(rm) => rm.fmt(f),
            Self::One => write!(f, "1"),
        }
    }
}

#[derive(strum_macros::Display)]
#[strum(serialize_all = "lowercase")]
pub enum NullaryOp {
    Xlat,
    Lahf,
    Sahf,
    Pushf,
    Popf,
    Aaa,
    Daa,
    Aas,
    Das,
    Aam,
    Aad,
    Cbw,
    Cwd,
    Movsb,
    Cmpsb,
    Scasb,
    Lodsb,
    Movsw,
    Cmpsw,
    Scasw,
    Lodsw,
    Stosb,
    Stosw,
    Int3,
    Into,
    Iret,
    Clc,
    Cmc,
    Stc,
    Cld,
    Std,
    Cli,
    Sti,
    Hlt,
    Wait,
}

#[derive(strum_macros::Display)]
#[strum(serialize_all = "lowercase")]
pub enum UnaryOp {
    Push,
    Pop,
    Inc,
    Dec,
    Neg,
    Mul,
    Imul,
    Div,
    Idiv,
    Not,
    Call,
    #[strum(serialize = "call far")]
    Callf,
    Jmp,
    #[strum(serialize = "jmp far")]
    Jmpf,
}

#[derive(strum_macros::Display)]
#[strum(serialize_all = "lowercase")]
pub enum BinaryOp {
    Mov,
    Add,
    Sub,
    Cmp,
    Xchg,
    In,
    Out,
    Lea,
    Lds,
    Les,
    Adc,
    Sbb,
    And,
    Test,
    Or,
    Xor,
}

#[derive(strum_macros::Display)]
#[strum(serialize_all = "lowercase")]
pub enum ShiftRotOp {
    Shl,
    Shr,
    Sar,
    Rol,
    Ror,
    Rcl,
    Rcr,
}

#[derive(strum_macros::Display)]
#[strum(serialize_all = "lowercase")]
pub enum JumpOp {
    Jnz,
    Je,
    Jl,
    Jle,
    Jb,
    Jbe,
    Jp,
    Jo,
    Js,
    Jnl,
    Jg,
    Jnb,
    Ja,
    Jnp,
    Jno,
    Jns,
    Loop,
    Loopz,
    Loopnz,
    Jcxz,
}

#[derive(strum_macros::Display)]
#[strum(serialize_all = "lowercase")]
pub enum ModifierOp {
    Rep,
    Lock,
}

#[derive(strum_macros::Display)]
#[strum(serialize_all = "lowercase")]
pub enum ControlXferOp {
    Ret,
    Retf,
    Int,
}

pub enum Instr {
    NullaryInstr {
        op: NullaryOp,
    },
    UnaryInstr {
        op: UnaryOp,
        dst: UnaryDest,
        wide: Option<bool>,
    },
    BinaryInstr {
        op: BinaryOp,
        dst: RM,
        src: RM,
    },
    ShiftRotInstr {
        op: ShiftRotOp,
        dst: RM,
        src: ShiftRotSrc,
        wide: bool,
    },
    JumpInstr {
        op: JumpOp,
        offset: Offset,
    },
    ModifierInstr {
        op: ModifierOp,
        instr: Box<Instr>,
    },
    ControlXferInstr {
        op: ControlXferOp,
        value: Option<Literal>,
    },
}

impl Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instr::NullaryInstr { op } => {
                write!(f, "{}", op)
            }
            Instr::UnaryInstr { op, dst, wide } => match wide {
                Some(true) => write!(f, "{} word {}", op, dst),
                Some(false) => write!(f, "{} byte {}", op, dst),
                _ => write!(f, "{} {}", op, dst),
            },
            Instr::BinaryInstr { op, dst, src } => {
                write!(f, "{} {}, {}", op, dst, src)
            }
            Instr::ShiftRotInstr { op, dst, src, wide } => {
                if *wide {
                    write!(f, "{} word {}, {}", op, dst, src)
                } else {
                    write!(f, "{} byte {}, {}", op, dst, src)
                }
            }
            Instr::JumpInstr { op, offset: inc } => {
                write!(f, "{} {}", op, inc)
            }
            Instr::ModifierInstr { op, instr } => {
                write!(f, "{} {}", op, instr)
            }
            Instr::ControlXferInstr { op, value } => {
                if let Some(value) = value {
                    write!(f, "{} {}", op, value)
                } else {
                    write!(f, "{}", op)
                }
            }
        }
    }
}

// TODO could be less sloppy with this
fn join_display_vec(items: Vec<Instr>) -> String {
    let mut result = String::new();

    for (i, item) in items.iter().enumerate() {
        if i > 0 {
            result.push('\n');
        }
        result.push_str(&format!("{}", item));
    }

    result
}

pub enum Offset {
    Byte(i8),
    Word(i16),
}

impl Display for Offset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let val = match self {
            Offset::Byte(val) => *val as i16,
            Offset::Word(val) => *val,
        };
        let sign = if val < 0 { "-" } else { "+" };
        let inc = (val + 2).abs();
        write!(f, "${}{}", sign, inc)
    }
}

#[derive(strum_macros::Display)]
#[strum(serialize_all = "lowercase")]
pub enum Register {
    AL,
    AX,
    CL,
    CX,
    DL,
    DX,
    BL,
    BX,
    AH,
    SP,
    CH,
    BP,
    DH,
    SI,
    BH,
    DI,
}

#[derive(strum_macros::Display)]
#[strum(serialize_all = "lowercase")]
pub enum Segment {
    CS,
    DS,
    ES,
    SS,
}

pub enum Literal {
    Byte(i8),
    Word(u16),
}

impl Literal {
    pub fn as_word(self) -> Self {
        match self {
            Self::Byte(x) => Self::Word(x as u16),
            Self::Word(x) => Self::Word(x),
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Byte(x) => write!(f, "{}", x),
            Self::Word(x) => write!(f, "{}", x),
        }
    }
}

pub struct MemAddr {
    segment: Option<Segment>,
    reg1: Option<Register>,
    reg2: Option<Register>,
    disp: Option<Literal>,
}

impl MemAddr {
    pub fn new(
        segment: Option<Segment>,
        reg1: Option<Register>,
        reg2: Option<Register>,
        disp: Option<Literal>,
    ) -> Self {
        if reg1.is_none() && reg2.is_none() && disp.is_none() {
            panic!("Have to have at least one Some for MemAddr attributes.");
        }
        Self {
            segment,
            reg1,
            reg2,
            disp,
        }
    }
}

impl Display for MemAddr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(seg) = self.segment.as_ref() {
            write!(f, "{}:", seg)?;
        }

        write!(f, "[")?;
        let mut is_first = true;

        if let Some(val) = self.reg1.as_ref() {
            write!(f, "{}", val)?;
            is_first = false;
        }
        if let Some(val) = self.reg2.as_ref() {
            if !is_first {
                write!(f, " + ")?;
            }
            write!(f, "{}", val)?;
            is_first = false;
        }

        if let Some(val) = self.disp.as_ref() {
            let is_neg = matches!(val, Literal::Byte(x) if *x < 0);
            let connector = match (is_first, is_neg) {
                (true, true) => "-",
                (true, false) => "",
                (false, true) => " - ",
                (false, false) => " + ",
            };
            match val {
                //Literal::Narrow(0) => (),
                //Literal::Wide(0) => (),
                Literal::Byte(x) => write!(f, "{}{}", connector, x.abs())?,
                Literal::Word(x) => write!(f, "{}{}", connector, x)?,
            }
        };

        write!(f, "]")
    }
}

fn parse_register(code: u8, wide: bool) -> Register {
    match code {
        0b000 => {
            if wide {
                Register::AX
            } else {
                Register::AL
            }
        }
        0b001 => {
            if wide {
                Register::CX
            } else {
                Register::CL
            }
        }
        0b010 => {
            if wide {
                Register::DX
            } else {
                Register::DL
            }
        }
        0b011 => {
            if wide {
                Register::BX
            } else {
                Register::BL
            }
        }
        0b100 => {
            if wide {
                Register::SP
            } else {
                Register::AH
            }
        }
        0b101 => {
            if wide {
                Register::BP
            } else {
                Register::CH
            }
        }
        0b110 => {
            if wide {
                Register::SI
            } else {
                Register::DH
            }
        }
        0b111 => {
            if wide {
                Register::DI
            } else {
                Register::BH
            }
        }
        _ => panic!("Unexpected register code {:?}", code),
    }
}

fn parse_segment(code: u8) -> Segment {
    match code {
        0b00 => Segment::ES,
        0b01 => Segment::CS,
        0b10 => Segment::SS,
        0b11 => Segment::DS,
        _ => panic!("Unexpected register code {:?}", code),
    }
}

fn parse_segment_instr(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, segment) = parse_code_segment(input, 0b001, 0b110)?;
    Ok((input, segment))
}

pub fn disassemble(path: &PathBuf) -> anyhow::Result<Vec<Instr>> {
    let data = std::fs::read(path)?;
    let input = data.as_slice();
    let mut it = iterator(input, parse_instr);
    let instrs = it.collect::<Vec<Instr>>();
    let (rest, _) = it.finish().unwrap();
    // TODO really should be a Result, not this panic.
    assert!(
        rest.is_empty(),
        "Did not parse whole input!\n{}",
        join_display_vec(instrs)
    );
    Ok(instrs)
}

#[derive(Debug)]
pub struct ParseError(Vec<u8>);

impl Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Parse error @ {}", print_vec_u8_bits(&self.0))
    }
}

impl std::error::Error for ParseError {}

pub enum Displacement {
    D8,
    D16,
}

pub enum MovMode {
    MemMode(Option<Displacement>),
    RegMode,
}

impl From<u8> for MovMode {
    fn from(code: u8) -> Self {
        match code {
            0b00 => MovMode::MemMode(None),
            0b01 => MovMode::MemMode(Some(Displacement::D8)),
            0b10 => MovMode::MemMode(Some(Displacement::D16)),
            0b11 => MovMode::RegMode,
            _ => panic!("invalid input code"),
        }
    }
}

/// Parse some instruction from the bits.
fn parse_instr(input: &[u8]) -> IResult<&[u8], Instr> {
    alt((
        // `NullaryInstr`s
        alt((
            alt((
                |input| parse_nullary_op(input, &[0b11010111], NullaryOp::Xlat),
                |input| parse_nullary_op(input, &[0b10011111], NullaryOp::Lahf),
                |input| parse_nullary_op(input, &[0b10011110], NullaryOp::Sahf),
                |input| parse_nullary_op(input, &[0b10011100], NullaryOp::Pushf),
                |input| parse_nullary_op(input, &[0b10011101], NullaryOp::Popf),
                |input| parse_nullary_op(input, &[0b00110111], NullaryOp::Aaa),
                |input| parse_nullary_op(input, &[0b00100111], NullaryOp::Daa),
                |input| parse_nullary_op(input, &[0b00111111], NullaryOp::Aas),
                |input| parse_nullary_op(input, &[0b00101111], NullaryOp::Das),
                |input| parse_nullary_op(input, &[0b00101111], NullaryOp::Das),
                |input| parse_nullary_op(input, &[0b10011000], NullaryOp::Cbw),
                |input| parse_nullary_op(input, &[0b10011001], NullaryOp::Cwd),
                |input| parse_nullary_op(input, &[0b11001100], NullaryOp::Int3),
                |input| parse_nullary_op(input, &[0b11010100, 0b00001010], NullaryOp::Aam),
                |input| parse_nullary_op(input, &[0b11010101, 0b00001010], NullaryOp::Aad),
            )),
            alt((
                |input| parse_nullary_op(input, &[0b10100100], NullaryOp::Movsb),
                |input| parse_nullary_op(input, &[0b10100110], NullaryOp::Cmpsb),
                |input| parse_nullary_op(input, &[0b10101110], NullaryOp::Scasb),
                |input| parse_nullary_op(input, &[0b10101100], NullaryOp::Lodsb),
                |input| parse_nullary_op(input, &[0b10100101], NullaryOp::Movsw),
                |input| parse_nullary_op(input, &[0b10100111], NullaryOp::Cmpsw),
                |input| parse_nullary_op(input, &[0b10101111], NullaryOp::Scasw),
                |input| parse_nullary_op(input, &[0b10101101], NullaryOp::Lodsw),
                |input| parse_nullary_op(input, &[0b10101010], NullaryOp::Stosb),
                |input| parse_nullary_op(input, &[0b10101011], NullaryOp::Stosw),
            )),
            alt((
                |input| parse_nullary_op(input, &[0b11001110], NullaryOp::Into),
                |input| parse_nullary_op(input, &[0b11001111], NullaryOp::Iret),
                |input| parse_nullary_op(input, &[0b11111000], NullaryOp::Clc),
                |input| parse_nullary_op(input, &[0b11110101], NullaryOp::Cmc),
                |input| parse_nullary_op(input, &[0b11111001], NullaryOp::Stc),
                |input| parse_nullary_op(input, &[0b11111100], NullaryOp::Cld),
                |input| parse_nullary_op(input, &[0b11111101], NullaryOp::Std),
                |input| parse_nullary_op(input, &[0b11111010], NullaryOp::Cli),
                |input| parse_nullary_op(input, &[0b11111011], NullaryOp::Sti),
                |input| parse_nullary_op(input, &[0b11110100], NullaryOp::Hlt),
                |input| parse_nullary_op(input, &[0b10011011], NullaryOp::Wait),
            )),
        )),
        // `UnaryInstr`s
        alt((
            |input| parse_unary_rm(input, 0b1111111, 0b110, UnaryOp::Push),
            |input| parse_unary_rm(input, 0b1000111, 0b000, UnaryOp::Pop),
            |input| parse_unary_rm(input, 0b1111111, 0b000, UnaryOp::Inc),
            |input| parse_unary_rm(input, 0b1111111, 0b001, UnaryOp::Dec),
            |input| parse_unary_rm(input, 0b1111011, 0b011, UnaryOp::Neg),
            |input| parse_unary_rm(input, 0b1111011, 0b100, UnaryOp::Mul),
            |input| parse_unary_rm(input, 0b1111011, 0b101, UnaryOp::Imul),
            |input| parse_unary_rm(input, 0b1111011, 0b110, UnaryOp::Div),
            |input| parse_unary_rm(input, 0b1111011, 0b111, UnaryOp::Idiv),
            |input| parse_unary_rm(input, 0b1111011, 0b010, UnaryOp::Not),
            |input| parse_unary_rm(input, 0b1111111, 0b010, UnaryOp::Call),
            |input| parse_unary_rm(input, 0b1111111, 0b011, UnaryOp::Callf),
            |input| parse_unary_rm(input, 0b1111111, 0b100, UnaryOp::Jmp),
            |input| parse_unary_rm(input, 0b1111111, 0b101, UnaryOp::Jmpf),
        )),
        alt((
            |input| parse_unary_reg(input, 0b01010, UnaryOp::Push),
            |input| parse_unary_reg(input, 0b01011, UnaryOp::Pop),
            |input| parse_unary_reg(input, 0b01000, UnaryOp::Inc),
            |input| parse_unary_reg(input, 0b01001, UnaryOp::Dec),
            |input| parse_unary_segreg(input, 0b000, 0b110, UnaryOp::Push),
            |input| parse_unary_segreg(input, 0b000, 0b111, UnaryOp::Pop),
            |input| parse_unary_interseg(input, 0b10011010, UnaryOp::Call),
            |input| parse_unary_interseg(input, 0b11101010, UnaryOp::Jmp),
            |input| parse_unary_offset(input, 0b11101001, UnaryOp::Jmp),
            |input| parse_unary_offset(input, 0b11101000, UnaryOp::Call),
        )),
        // `BinaryInstr`s
        alt((
            parse_mov_imd_to_reg,
            |input| parse_rm_to_rm(input, 0b100010, false, BinaryOp::Mov),
            |input| parse_rm_to_rm(input, 0b000000, false, BinaryOp::Add),
            |input| parse_rm_to_rm(input, 0b001010, false, BinaryOp::Sub),
            |input| parse_rm_to_rm(input, 0b001110, false, BinaryOp::Cmp),
            |input| parse_rm_to_rm(input, 0b000100, false, BinaryOp::Adc),
            |input| parse_rm_to_rm(input, 0b000110, false, BinaryOp::Sbb),
            |input| parse_rm_to_rm(input, 0b001000, false, BinaryOp::And),
            |input| parse_rm_to_rm(input, 0b000100, false, BinaryOp::Test),
            |input| parse_rm_to_rm(input, 0b000010, false, BinaryOp::Or),
            |input| parse_rm_to_rm(input, 0b001100, false, BinaryOp::Xor),
            |input| parse_rm_to_rm(input, 0b1000011, true, BinaryOp::Xchg),
            |input| parse_rm_to_rm(input, 0b1000010, true, BinaryOp::Test),
            |input| parse_binary_segreg(input, 0b10001100, false, BinaryOp::Mov),
        )),
        alt((
            |input| parse_load(input, 0b10001101, BinaryOp::Lea),
            |input| parse_load(input, 0b11000101, BinaryOp::Lds),
            |input| parse_load(input, 0b11000100, BinaryOp::Les),
        )),
        alt((
            |input| parse_imd_to_rm(input, 0b110001, 0b000, true, BinaryOp::Mov),
            |input| parse_imd_to_rm(input, 0b100000, 0b000, false, BinaryOp::Add),
            |input| parse_imd_to_rm(input, 0b100000, 0b101, false, BinaryOp::Sub),
            |input| parse_imd_to_rm(input, 0b100000, 0b111, false, BinaryOp::Cmp),
            |input| parse_imd_to_rm(input, 0b100000, 0b010, false, BinaryOp::Adc),
            |input| parse_imd_to_rm(input, 0b100000, 0b011, false, BinaryOp::Sbb),
            |input| parse_imd_to_rm(input, 0b100000, 0b100, false, BinaryOp::And),
            |input| parse_imd_to_rm(input, 0b111101, 0b000, false, BinaryOp::Test),
            |input| parse_imd_to_rm(input, 0b100000, 0b001, false, BinaryOp::Or),
            |input| parse_imd_to_rm(input, 0b100000, 0b110, false, BinaryOp::Xor),
        )),
        alt((
            // TODO do I really need 2 of these for `Mov`? maybe can just get it from the last bit.
            |input| parse_accum_style(input, 0b1010000, true, BinaryOp::Mov),
            |input| parse_accum_style(input, 0b1010001, false, BinaryOp::Mov),
            |input| parse_accum_style(input, 0b0000010, true, BinaryOp::Add),
            |input| parse_accum_style(input, 0b0010110, true, BinaryOp::Sub),
            |input| parse_accum_style(input, 0b0011110, true, BinaryOp::Cmp),
            |input| parse_accum_style(input, 0b0001010, true, BinaryOp::Adc),
            |input| parse_accum_style(input, 0b0001110, true, BinaryOp::Sbb),
            |input| parse_accum_style(input, 0b0010010, true, BinaryOp::And),
            |input| parse_accum_style(input, 0b1010100, true, BinaryOp::Test),
            |input| parse_accum_style(input, 0b0000110, true, BinaryOp::Or),
            |input| parse_accum_style(input, 0b0011010, true, BinaryOp::Xor),
            |input| parse_accum_reg(input, 0b10010, BinaryOp::Xchg),
            // TODO you get a warning about negative literals for `In`, among others
            |input| parse_fixed_port(input, 0b1110010, true, BinaryOp::In),
            |input| parse_fixed_port(input, 0b1110011, false, BinaryOp::Out),
            |input| parse_variable_port(input, 0b1110110, true, BinaryOp::In),
            |input| parse_variable_port(input, 0b1110111, false, BinaryOp::Out),
        )),
        alt((
            |input| parse_shift_rotate(input, 0b110100, 0b100, ShiftRotOp::Shl),
            |input| parse_shift_rotate(input, 0b110100, 0b101, ShiftRotOp::Shr),
            |input| parse_shift_rotate(input, 0b110100, 0b111, ShiftRotOp::Sar),
            |input| parse_shift_rotate(input, 0b110100, 0b000, ShiftRotOp::Rol),
            |input| parse_shift_rotate(input, 0b110100, 0b001, ShiftRotOp::Ror),
            |input| parse_shift_rotate(input, 0b110100, 0b010, ShiftRotOp::Rcl),
            |input| parse_shift_rotate(input, 0b110100, 0b011, ShiftRotOp::Rcr),
        )),
        // `JumpInstr`s
        alt((
            |input| parse_jump_instr(input, 0b01110000, JumpOp::Jo),
            |input| parse_jump_instr(input, 0b01110001, JumpOp::Jno),
            |input| parse_jump_instr(input, 0b01110010, JumpOp::Jb),
            |input| parse_jump_instr(input, 0b01110011, JumpOp::Jnb),
            |input| parse_jump_instr(input, 0b01110100, JumpOp::Je),
            |input| parse_jump_instr(input, 0b01110101, JumpOp::Jnz),
            |input| parse_jump_instr(input, 0b01110110, JumpOp::Jbe),
            |input| parse_jump_instr(input, 0b01110111, JumpOp::Ja),
            |input| parse_jump_instr(input, 0b01111000, JumpOp::Js),
            |input| parse_jump_instr(input, 0b01111001, JumpOp::Jns),
            |input| parse_jump_instr(input, 0b01111010, JumpOp::Jp),
            |input| parse_jump_instr(input, 0b01111011, JumpOp::Jnp),
            |input| parse_jump_instr(input, 0b01111100, JumpOp::Jl),
            |input| parse_jump_instr(input, 0b01111101, JumpOp::Jnl),
            |input| parse_jump_instr(input, 0b01111110, JumpOp::Jle),
            |input| parse_jump_instr(input, 0b01111111, JumpOp::Jg),
            |input| parse_jump_instr(input, 0b11100000, JumpOp::Loopnz),
            |input| parse_jump_instr(input, 0b11100001, JumpOp::Loopz),
            |input| parse_jump_instr(input, 0b11100010, JumpOp::Loop),
            |input| parse_jump_instr(input, 0b11100011, JumpOp::Jcxz),
        )),
        // `ModifierInstr`s
        alt((
            |input| parse_repeat_instr(input, 0b1111001, ModifierOp::Rep),
            // TODO actually an 8bit opcode... although it is "not used."
            |input| parse_repeat_instr(input, 0b1111000, ModifierOp::Lock),
        )),
        // `ControlXferInstr`s
        alt((
            |input| parse_control_xfer_nullary_op(input, 0b11000011, ControlXferOp::Ret),
            |input| parse_control_xfer_nullary_op(input, 0b11001011, ControlXferOp::Retf),
        )),
        alt((
            |input| parse_control_xfer(input, 0b11000010, ControlXferOp::Ret, true),
            |input| parse_control_xfer(input, 0b11001101, ControlXferOp::Int, false),
            |input| parse_control_xfer(input, 0b11001010, ControlXferOp::Retf, true),
        )),
    ))(input)
}

fn parse_nullary_op<'a, 'b>(
    input: &'a [u8],
    opcode: &'b [u8],
    op: NullaryOp,
) -> IResult<&'a [u8], Instr> {
    let (input, _) = tag_bytes(opcode)(input)?;
    Ok((input, Instr::NullaryInstr { op }))
}

fn parse_mod_reg_rm_byte(input: &[u8], wide: bool) -> IResult<&[u8], (MovMode, Register, u8)> {
    bits::<_, _, nom::error::Error<(&[u8], usize)>, _, _>(tuple((
        map(take(2usize), |code: u8| code.into()),
        map(take(3usize), |code: u8| parse_register(code, wide)),
        take(3usize),
    )))(input)
}

fn parse_6bits_and_flags(input: &[u8], opcode: u8) -> IResult<&[u8], (bool, bool)> {
    let (input, (_, a, b)): (&[u8], (u8, bool, bool)) =
        bits::<_, _, nom::error::Error<(&[u8], usize)>, _, _>(tuple((
            tag(opcode, 6usize),
            map(take(1usize), |x: u8| x == 1),
            map(take(1usize), |x: u8| x == 1),
        )))(input)?;
    Ok((input, (a, b)))
}

fn parse_7bits_and_flag(input: &[u8], opcode: u8) -> IResult<&[u8], bool> {
    let (input, (_, a)): (&[u8], (u8, bool)) = bits::<_, _, nom::error::Error<(&[u8], usize)>, _, _>(
        tuple((tag(opcode, 7usize), map(take(1usize), |x: u8| x == 1))),
    )(input)?;
    Ok((input, a))
}

fn parse_mod_code_rm(input: &[u8], opcode: u8) -> IResult<&[u8], (MovMode, u8)> {
    let (input, (mode, _, rm)): (&[u8], (MovMode, u8, u8)) =
        bits::<_, _, nom::error::Error<(&[u8], usize)>, _, _>(tuple((
            map(take(2usize), |code: u8| code.into()),
            tag(opcode, 3usize),
            take(3usize),
        )))(input)?;
    Ok((input, (mode, rm)))
}

fn parse_code_register(input: &[u8], opcode: u8) -> IResult<&[u8], Register> {
    let (input, (_, reg)): (&[u8], (u8, Register)) =
        bits::<_, _, nom::error::Error<(&[u8], usize)>, _, _>(tuple((
            tag(opcode, 5usize),
            map(take(3usize), |code: u8| parse_register(code, true)),
        )))(input)?;
    Ok((input, reg))
}

fn parse_code_segment(input: &[u8], opcode1: u8, opcode2: u8) -> IResult<&[u8], Segment> {
    let (input, (_, segment, _)): (&[u8], (u8, Segment, u8)) =
        bits::<_, _, nom::error::Error<(&[u8], usize)>, _, _>(tuple((
            tag(opcode1, 3usize),
            map(take(2usize), |code: u8| parse_segment(code)),
            tag(opcode2, 3usize),
        )))(input)?;
    Ok((input, segment))
}

/// Parse register/memory/immediate to/from register/memory instructions.
fn parse_rm_to_rm(
    input: &[u8],
    opcode: u8,
    parse_seven: bool,
    op: BinaryOp,
) -> IResult<&[u8], Instr> {
    let (input, segment) = opt(parse_segment_instr)(input)?;

    // First byte
    let (input, destination, wide) = if parse_seven {
        let (input, wide) = parse_7bits_and_flag(input, opcode)?;
        // TODO kinda gross!
        let destination = match op {
            BinaryOp::Test => false,
            _ => true,
        };
        (input, destination, wide)
    } else {
        let (input, (destination, wide)) = parse_6bits_and_flags(input, opcode)?;
        (input, destination, wide)
    };

    // Second byte
    let (input, (mode, reg, rm)) = parse_mod_reg_rm_byte(input, wide)?;

    let (input, rm) = parse_rm(input, &mode, wide, rm, segment)?;
    if destination {
        Ok((
            input,
            Instr::BinaryInstr {
                op,
                dst: RM::Reg(reg),
                src: rm,
            },
        ))
    } else {
        Ok((
            input,
            Instr::BinaryInstr {
                op,
                dst: rm.try_into().unwrap(),
                src: RM::Reg(reg),
            },
        ))
    }
}

fn parse_load(input: &[u8], opcode: u8, op: BinaryOp) -> IResult<&[u8], Instr> {
    let (input, _) = tag_bytes([opcode])(input)?;
    let (input, (mode, reg, rm)) = parse_mod_reg_rm_byte(input, true)?;
    let (input, rm) = parse_rm(input, &mode, true, rm, None)?;
    Ok((
        input,
        Instr::BinaryInstr {
            op,
            dst: RM::Reg(reg),
            src: rm,
        },
    ))
}

/// Parse immediate to register instructions.
fn parse_mov_imd_to_reg(input: &[u8]) -> IResult<&[u8], Instr> {
    let (input, (_, wide, reg)): (&[u8], (u8, bool, u8)) =
        bits::<_, _, nom::error::Error<(&[u8], usize)>, _, _>(tuple((
            tag(0b1011, 4usize),
            map(take(1usize), |x: u8| x == 1),
            take(3usize),
        )))(input)?;

    let (input, imd) = if wide {
        parse_literal_word(input)?
    } else {
        parse_literal_byte(input)?
    };
    let instr = Instr::BinaryInstr {
        op: BinaryOp::Mov,
        dst: RM::Reg(parse_register(reg, wide)),
        src: RM::Imd(imd),
    };
    Ok((input, instr))
}

fn parse_imd_to_rm(
    input: &[u8],
    opcode1: u8,
    opcode2: u8,
    ignore_s: bool, // TODO maybe just need to check if op is MOV?
    op: BinaryOp,
) -> IResult<&[u8], Instr> {
    let (input, segment) = opt(parse_segment_instr)(input)?;

    // First byte
    let (input, (s, w)) = parse_6bits_and_flags(input, opcode1)?;

    // Second byte
    let (input, (mode, rm)) = parse_mod_code_rm(input, opcode2)?;

    let (input, rm) = parse_rm(input, &mode, w, rm, segment)?;

    let (input, data) = if w && (ignore_s || !s) {
        let (input, literal) = parse_literal_word(input)?;
        (input, literal)
    } else {
        let (input, mut literal) = parse_literal_byte(input)?;
        if w {
            literal = literal.as_word();
        }
        (input, literal)
    };

    Ok((
        input,
        Instr::BinaryInstr {
            op,
            dst: rm.try_into().unwrap(),
            src: RM::Imd(data),
        },
    ))
}

/// Helper for extracting similar accumulator-style MOVs.
fn parse_accum_style(
    input: &[u8],
    code: u8,
    destination: bool,
    op: BinaryOp,
) -> IResult<&[u8], Instr> {
    // First byte
    let (input, wide) = parse_7bits_and_flag(input, code)?;

    let (input, data) = if wide {
        parse_literal_word(input)?
    } else {
        parse_literal_byte(input)?
    };
    let reg = if wide { Register::AX } else { Register::AL };
    match (destination, &op) {
        (true, BinaryOp::Mov) => Ok((
            input,
            Instr::BinaryInstr {
                op,
                dst: RM::Reg(reg),
                src: RM::Mem(MemAddr::new(None, None, None, Some(data))),
            },
        )),
        (false, BinaryOp::Mov) => Ok((
            input,
            Instr::BinaryInstr {
                op,
                dst: RM::Mem(MemAddr::new(None, None, None, Some(data))),
                src: RM::Reg(reg),
            },
        )),
        _ => Ok((
            input,
            Instr::BinaryInstr {
                op,
                dst: RM::Reg(reg),
                src: RM::Imd(data),
            },
        )),
    }
}

// TODO very similar to `parse_unary_reg`.
fn parse_accum_reg(input: &[u8], opcode: u8, op: BinaryOp) -> IResult<&[u8], Instr> {
    let (input, reg) = parse_code_register(input, opcode)?;
    Ok((
        input,
        Instr::BinaryInstr {
            op,
            dst: RM::Reg(Register::AX),
            src: RM::Reg(reg),
        },
    ))
}

/// Parse the single next byte as a "literal."
fn parse_literal_byte(input: &[u8]) -> IResult<&[u8], Literal> {
    map(take_bytes(1usize), |x: &[u8]| Literal::Byte(x[0] as i8))(input)
}

fn parse_word(input: &[u8]) -> IResult<&[u8], u16> {
    map(take_bytes(2usize), |x: &[u8]| {
        (x[1] as u16) << 8 | (x[0] as u16)
    })(input)
}

/// Parse the next two bytes as a "literal."
fn parse_literal_word(input: &[u8]) -> IResult<&[u8], Literal> {
    map(parse_word, |x: u16| Literal::Word(x))(input)
}

fn parse_effective_address(
    mode: &MovMode,
    code: u8,
    disp: Option<Literal>,
    segment: Option<Segment>,
) -> MemAddr {
    match code {
        0b000 => MemAddr::new(segment, Some(Register::BX), Some(Register::SI), disp),
        0b001 => MemAddr::new(segment, Some(Register::BX), Some(Register::DI), disp),
        0b010 => MemAddr::new(segment, Some(Register::BP), Some(Register::SI), disp),
        0b011 => MemAddr::new(segment, Some(Register::BP), Some(Register::DI), disp),
        0b100 => MemAddr::new(segment, Some(Register::SI), None, disp),
        0b101 => MemAddr::new(segment, Some(Register::DI), None, disp),
        0b110 => match mode {
            MovMode::MemMode(None) => MemAddr::new(segment, None, None, disp),
            _ => MemAddr::new(segment, Some(Register::BP), None, disp),
        },
        0b111 => MemAddr::new(segment, Some(Register::BX), None, disp),
        _ => panic!("invalid input code"),
    }
}

fn parse_rm<'a, 'b>(
    input: &'a [u8],
    mode: &'b MovMode,
    wide: bool,
    rm: u8,
    segment: Option<Segment>,
) -> IResult<&'a [u8], RM> {
    match mode {
        MovMode::MemMode(disp) => {
            let (input, disp) = match disp {
                None => {
                    if rm == 0b110 {
                        let (input, literal) = parse_literal_word(input)?;
                        (input, Some(literal))
                    } else {
                        (input, None)
                    }
                }
                Some(Displacement::D8) => {
                    let (input, literal) = parse_literal_byte(input)?;
                    (input, Some(literal))
                }
                Some(Displacement::D16) => {
                    let (input, literal) = parse_literal_word(input)?;
                    (input, Some(literal))
                }
            };
            let mem_addr = parse_effective_address(mode, rm, disp, segment);
            Ok((input, RM::Mem(mem_addr)))
        }
        MovMode::RegMode => {
            let register = parse_register(rm, wide);
            Ok((input, RM::Reg(register)))
        }
    }
}

fn parse_jump_instr(input: &[u8], opcode: u8, op: JumpOp) -> IResult<&[u8], Instr> {
    let (input, _) = tag_bytes([opcode])(input)?;
    let (input, inc) = take_bytes(1usize)(input)?;
    Ok((
        input,
        Instr::JumpInstr {
            op,
            offset: Offset::Byte(inc[0] as i8),
        },
    ))
}

fn parse_unary_rm(input: &[u8], opcode1: u8, opcode2: u8, op: UnaryOp) -> IResult<&[u8], Instr> {
    let (input, segment) = opt(parse_segment_instr)(input)?;
    let (input, w) = parse_7bits_and_flag(input, opcode1)?;
    let (input, (mode, rm)) = parse_mod_code_rm(input, opcode2)?;
    let (input, rm) = parse_rm(input, &mode, w, rm, segment)?;

    Ok((
        input,
        Instr::UnaryInstr {
            op,
            dst: UnaryDest::RM(rm),
            wide: Some(w),
        },
    ))
}

fn parse_unary_reg(input: &[u8], opcode: u8, op: UnaryOp) -> IResult<&[u8], Instr> {
    let (input, reg) = parse_code_register(input, opcode)?;
    Ok((
        input,
        Instr::UnaryInstr {
            op,
            dst: UnaryDest::RM(RM::Reg(reg)),
            wide: None,
        },
    ))
}

fn parse_unary_segreg(
    input: &[u8],
    opcode1: u8,
    opcode2: u8,
    op: UnaryOp,
) -> IResult<&[u8], Instr> {
    let (input, segreg) = parse_code_segment(input, opcode1, opcode2)?;
    Ok((
        input,
        Instr::UnaryInstr {
            op,
            dst: UnaryDest::RM(RM::SegReg(segreg)),
            wide: None,
        },
    ))
}

// TODO these next two are quite similar...
fn parse_fixed_port(
    input: &[u8],
    opcode: u8,
    destination: bool,
    op: BinaryOp,
) -> IResult<&[u8], Instr> {
    let (input, wide) = parse_7bits_and_flag(input, opcode)?;
    let (input, data) = parse_literal_byte(input)?;
    let reg = if wide { Register::AX } else { Register::AL };

    let instr = if destination {
        Instr::BinaryInstr {
            op,
            dst: RM::Reg(reg),
            src: RM::Imd(data),
        }
    } else {
        Instr::BinaryInstr {
            op,
            dst: RM::Imd(data),
            src: RM::Reg(reg),
        }
    };
    Ok((input, instr))
}

fn parse_variable_port(
    input: &[u8],
    opcode: u8,
    destination: bool,
    op: BinaryOp,
) -> IResult<&[u8], Instr> {
    let (input, wide) = parse_7bits_and_flag(input, opcode)?;
    let reg = if wide {
        RM::Reg(Register::AX)
    } else {
        RM::Reg(Register::AL)
    };
    let instr = if destination {
        Instr::BinaryInstr {
            op,
            dst: reg,
            src: RM::Reg(Register::DX),
        }
    } else {
        Instr::BinaryInstr {
            op,
            dst: RM::Reg(Register::DX),
            src: reg,
        }
    };
    Ok((input, instr))
}

fn parse_shift_rotate(
    input: &[u8],
    opcode1: u8,
    opcode2: u8,
    op: ShiftRotOp,
) -> IResult<&[u8], Instr> {
    let (input, (v, wide)) = parse_6bits_and_flags(input, opcode1)?;
    let (input, (mode, rm)) = parse_mod_code_rm(input, opcode2)?;
    let (input, rm) = parse_rm(input, &mode, wide, rm, None)?;
    let src = if v {
        ShiftRotSrc::RM(RM::Reg(Register::CL))
    } else {
        ShiftRotSrc::One
    };
    let instr = Instr::ShiftRotInstr {
        op,
        dst: rm.try_into().unwrap(),
        src,
        wide,
    };
    Ok((input, instr))
}

fn parse_repeat_instr(input: &[u8], opcode: u8, op: ModifierOp) -> IResult<&[u8], Instr> {
    let (input, _z) = parse_7bits_and_flag(input, opcode)?;
    let (input, instr) = parse_instr(input)?;
    let instr = Instr::ModifierInstr {
        op,
        instr: Box::new(instr),
    };
    Ok((input, instr))
}

fn parse_control_xfer_nullary_op(
    input: &[u8],
    opcode: u8,
    op: ControlXferOp,
) -> IResult<&[u8], Instr> {
    let (input, _) = tag_bytes([opcode])(input)?;
    Ok((input, Instr::ControlXferInstr { op, value: None }))
}

fn parse_control_xfer(
    input: &[u8],
    opcode: u8,
    op: ControlXferOp,
    wide: bool,
) -> IResult<&[u8], Instr> {
    let (input, _) = tag_bytes([opcode])(input)?;
    let (input, imd) = if wide {
        parse_literal_word(input)?
    } else {
        parse_literal_byte(input)?
    };
    Ok((
        input,
        Instr::ControlXferInstr {
            op,
            value: Some(imd),
        },
    ))
}

fn parse_unary_interseg(input: &[u8], opcode: u8, op: UnaryOp) -> IResult<&[u8], Instr> {
    let (input, _) = tag_bytes([opcode])(input)?;
    let (input, end) = parse_literal_word(input)?;
    let (input, start) = parse_literal_word(input)?;
    let instr = Instr::UnaryInstr {
        op,
        dst: UnaryDest::InterSeg((start, end)),
        wide: None,
    };
    Ok((input, instr))
}

fn parse_binary_segreg(
    input: &[u8],
    opcode: u8,
    dest: bool,
    op: BinaryOp,
) -> IResult<&[u8], Instr> {
    let (input, _) = tag_bytes([opcode])(input)?;
    let (input, (mode, _, segreg, rm)): (&[u8], (MovMode, u8, Segment, u8)) =
        bits::<_, _, nom::error::Error<(&[u8], usize)>, _, _>(tuple((
            map(take(2usize), |code: u8| code.into()),
            tag(0b0, 1usize),
            map(take(2usize), |code: u8| parse_segment(code)),
            take(3usize),
        )))(input)?;

    let (input, rm) = parse_rm(input, &mode, true, rm, None)?;
    if dest {
        Ok((
            input,
            Instr::BinaryInstr {
                op,
                dst: RM::SegReg(segreg),
                src: rm,
            },
        ))
    } else {
        Ok((
            input,
            Instr::BinaryInstr {
                op,
                dst: rm.try_into().unwrap(),
                src: RM::SegReg(segreg),
            },
        ))
    }
}

fn parse_unary_offset(input: &[u8], opcode: u8, op: UnaryOp) -> IResult<&[u8], Instr> {
    let (input, _) = tag_bytes([opcode])(input)?;
    let (input, offset) = parse_word(input)?;
    let instr = Instr::UnaryInstr {
        op,
        dst: UnaryDest::Offset(Offset::Word(offset as i16)),
        wide: None,
    };
    Ok((input, instr))
}
