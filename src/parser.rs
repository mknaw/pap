use std::convert::TryFrom;
use std::fmt::{self, Display};
use std::path::PathBuf;

use nom::bits::bits;
use nom::bits::complete::{tag, take};
use nom::branch::alt;
use nom::bytes::complete::{tag as tag_bytes, take as take_bytes};
use nom::combinator::{iterator, map};
use nom::sequence::tuple;
use nom::IResult;

use crate::utils::print_vec_u8_bits;

pub enum RM {
    Reg(Register),
    Mem(MemAddr),
    Imd(Literal),
}

impl Display for RM {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RM::Reg(reg) => write!(f, "{}", reg),
            RM::Mem(ea) => write!(f, "{}", ea),
            RM::Imd(imd) => match imd {
                Literal::Byte(x) => write!(f, "byte {}", x),
                Literal::Word(x) => write!(f, "word {}", x),
            },
        }
    }
}

pub enum Dest {
    Reg(Register),
    Mem(MemAddr),
}

impl Display for Dest {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Dest::Reg(reg) => write!(f, "{}", reg),
            Dest::Mem(ea) => write!(f, "{}", ea),
        }
    }
}

impl TryFrom<RM> for Dest {
    type Error = String;

    fn try_from(value: RM) -> Result<Self, Self::Error> {
        match value {
            RM::Reg(register) => Ok(Dest::Reg(register)),
            RM::Mem(memaddr) => Ok(Dest::Mem(memaddr)),
            RM::Imd(_) => Err("Cannot convert `Imd` variant to `MovDest`.".to_string()),
        }
    }
}

#[derive(strum_macros::Display)]
#[strum(serialize_all = "lowercase")]
pub enum BinaryOp {
    Mov,
    Add,
    Sub,
    Cmp,
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

pub enum Instr {
    BinaryInstr { op: BinaryOp, dst: Dest, src: RM },
    JumpInstr { op: JumpOp, inc: Incr },
}

impl Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instr::BinaryInstr { op, dst, src } => {
                write!(f, "{} {}, {}", op, dst, src)
            }
            Instr::JumpInstr { op, inc } => {
                write!(f, "{} {}", op, inc)
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

pub struct Incr(i8);

impl Display for Incr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let sign = if self.0 < 0 { "-" } else { "+" };
        let inc = (self.0 + 2).abs();
        write!(f, "${}{}", sign, inc)
    }
}

#[derive(Debug)]
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

impl Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", format!("{:?}", self).to_lowercase())
    }
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
    reg1: Option<Register>,
    reg2: Option<Register>,
    disp: Option<Literal>,
}

impl MemAddr {
    pub fn new(reg1: Option<Register>, reg2: Option<Register>, disp: Option<Literal>) -> Self {
        if reg1.is_none() && reg2.is_none() && disp.is_none() {
            panic!("Have to have at least one Some for MemAddr attributes.");
        }
        Self { reg1, reg2, disp }
    }
}

impl Display for MemAddr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

fn parse_register(wide: bool, code: u8) -> Register {
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
        alt((
            parse_mov_imd_to_reg,
            move |input| parse_rm_to_rm(input, 0b100010, BinaryOp::Mov),
            move |input| parse_rm_to_rm(input, 0b000000, BinaryOp::Add),
            move |input| parse_rm_to_rm(input, 0b001010, BinaryOp::Sub),
            move |input| parse_rm_to_rm(input, 0b001110, BinaryOp::Cmp),
            move |input| parse_imd_to_rm(input, 0b110001, 0b000, true, BinaryOp::Mov),
            move |input| parse_imd_to_rm(input, 0b100000, 0b000, false, BinaryOp::Add),
            move |input| parse_imd_to_rm(input, 0b100000, 0b101, false, BinaryOp::Sub),
            move |input| parse_imd_to_rm(input, 0b100000, 0b111, false, BinaryOp::Cmp),
            // TODO do I really need 2 of these for `Mov`? maybe can just get it from the last bit.
            move |input| parse_accum_style(input, 0b1010000, true, BinaryOp::Mov),
            move |input| parse_accum_style(input, 0b1010001, false, BinaryOp::Mov),
            move |input| parse_accum_style(input, 0b0000010, true, BinaryOp::Add),
            move |input| parse_accum_style(input, 0b0010110, true, BinaryOp::Sub),
            move |input| parse_accum_style(input, 0b0011110, true, BinaryOp::Cmp),
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
    ))(input)
}

/// Parse register/memory/immediate to/from register/memory instructions.
fn parse_rm_to_rm(input: &[u8], flag: u8, op: BinaryOp) -> IResult<&[u8], Instr> {
    // First byte
    let (input, (_, destination, wide)): (&[u8], (u8, bool, bool)) =
        bits::<_, _, nom::error::Error<(&[u8], usize)>, _, _>(tuple((
            tag(flag, 6usize),
            map(take(1usize), |x: u8| x == 1),
            map(take(1usize), |x: u8| x == 1),
        )))(input)?;

    // Second byte
    let (input, (mode, reg, rm)): (&[u8], (MovMode, Register, u8)) =
        bits::<_, _, nom::error::Error<(&[u8], usize)>, _, _>(tuple((
            map(take(2usize), |code: u8| code.into()),
            map(take(3usize), |code: u8| parse_register(wide, code)),
            take(3usize),
        )))(input)?;

    let (input, rm) = parse_rm(input, &mode, wide, rm)?;
    if destination {
        Ok((
            input,
            Instr::BinaryInstr {
                op,
                dst: Dest::Reg(reg),
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
        dst: Dest::Reg(parse_register(wide, reg)),
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
    // First byte
    let (input, (_, s, w)): (&[u8], (u8, bool, bool)) =
        bits::<_, _, nom::error::Error<(&[u8], usize)>, _, _>(tuple((
            tag(opcode1, 6usize),
            map(take(1usize), |x: u8| x == 1),
            map(take(1usize), |x: u8| x == 1),
        )))(input)?;

    // Second byte
    let (input, (mode, _, rm)): (&[u8], (MovMode, u8, u8)) =
        bits::<_, _, nom::error::Error<(&[u8], usize)>, _, _>(tuple((
            map(take(2usize), |code: u8| code.into()),
            tag(opcode2, 3usize),
            take(3usize),
        )))(input)?;

    let (input, rm) = parse_rm(input, &mode, w, rm)?;

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
    let (input, (_, wide)): (&[u8], (u8, bool)) =
        bits::<_, _, nom::error::Error<(&[u8], usize)>, _, _>(tuple((
            tag(code, 7usize),
            map(take(1usize), |x: u8| x == 1),
        )))(input)?;

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
                dst: Dest::Reg(reg),
                src: RM::Mem(MemAddr::new(None, None, Some(data))),
            },
        )),
        (false, BinaryOp::Mov) => Ok((
            input,
            Instr::BinaryInstr {
                op,
                dst: Dest::Mem(MemAddr::new(None, None, Some(data))),
                src: RM::Reg(reg),
            },
        )),
        _ => Ok((
            input,
            Instr::BinaryInstr {
                op,
                dst: Dest::Reg(reg),
                src: RM::Imd(data),
            },
        )),
    }
}

/// Parse the single next byte as a "literal."
fn parse_literal_byte(input: &[u8]) -> IResult<&[u8], Literal> {
    map(take_bytes(1usize), |x: &[u8]| Literal::Byte(x[0] as i8))(input)
}

/// Parse the next two bytes as a "literal."
fn parse_literal_word(input: &[u8]) -> IResult<&[u8], Literal> {
    map(take_bytes(2usize), |x: &[u8]| {
        Literal::Word((x[1] as u16) << 8 | (x[0] as u16))
    })(input)
}

fn parse_effective_address(mode: &MovMode, code: u8, disp: Option<Literal>) -> MemAddr {
    match code {
        0b000 => MemAddr::new(Some(Register::BX), Some(Register::SI), disp),
        0b001 => MemAddr::new(Some(Register::BX), Some(Register::DI), disp),
        0b010 => MemAddr::new(Some(Register::BP), Some(Register::SI), disp),
        0b011 => MemAddr::new(Some(Register::BP), Some(Register::DI), disp),
        0b100 => MemAddr::new(Some(Register::SI), None, disp),
        0b101 => MemAddr::new(Some(Register::DI), None, disp),
        0b110 => match mode {
            MovMode::MemMode(None) => MemAddr::new(None, None, disp),
            _ => MemAddr::new(Some(Register::BP), None, disp),
        },
        0b111 => MemAddr::new(Some(Register::BX), None, disp),
        _ => panic!("invalid input code"),
    }
}

fn parse_rm<'a, 'b>(
    input: &'a [u8],
    mode: &'b MovMode,
    wide: bool,
    rm: u8,
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
            let mem_addr = parse_effective_address(mode, rm, disp);
            Ok((input, RM::Mem(mem_addr)))
        }
        MovMode::RegMode => {
            let register = parse_register(wide, rm);
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
            inc: Incr(inc[0] as i8),
        },
    ))
}
