use std::convert::TryFrom;
use std::fmt::{self, Display};
use std::path::PathBuf;

use nom::bits::bits;
use nom::bits::complete::{tag, take};
use nom::branch::alt;
use nom::bytes::complete::take as take_bytes;
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
                Literal::Narrow(x) => write!(f, "byte {}", x),
                Literal::Wide(x) => write!(f, "word {}", x),
            },
        }
    }
}

pub enum MovDest {
    Reg(Register),
    Mem(MemAddr),
}

impl Display for MovDest {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MovDest::Reg(reg) => write!(f, "{}", reg),
            MovDest::Mem(ea) => write!(f, "{}", ea),
        }
    }
}

impl TryFrom<RM> for MovDest {
    type Error = String;

    fn try_from(value: RM) -> Result<Self, Self::Error> {
        match value {
            RM::Reg(register) => Ok(MovDest::Reg(register)),
            RM::Mem(memaddr) => Ok(MovDest::Mem(memaddr)),
            RM::Imd(_) => Err("Cannot convert `Imd` variant to `MovDest`.".to_string()),
        }
    }
}

pub enum Instr {
    Mov { dst: MovDest, src: RM },
}

impl Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instr::Mov { dst, src } => {
                write!(f, "mov {}, {}", dst, src)
            }
        }
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
    Narrow(i8),
    Wide(u16),
}

impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Narrow(x) => write!(f, "{}", x),
            Self::Wide(x) => write!(f, "{}", x),
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
            let is_neg = matches!(val, Literal::Narrow(x) if *x < 0);
            let connector = match (is_first, is_neg) {
                (true, true) => "-",
                (true, false) => "",
                (false, true) => " - ",
                (false, false) => " + ",
            };
            match val {
                Literal::Narrow(x) => write!(f, "{}{}", connector, x.abs())?,
                Literal::Wide(x) => write!(f, "{}{}", connector, x)?,
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
    assert!(rest.is_empty(), "Did not parse whole input!");
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
        parse_mov,
        parse_mov_immediate,
        parse_mov_immediate_to_reg_or_mem,
        parse_mov_mem_to_accum,
        parse_mov_accum_to_mem,
    ))(input)
}

/// Parse register/memory/immediate to/from register/memory instructions.
fn parse_mov(input: &[u8]) -> IResult<&[u8], Instr> {
    // First byte
    let (input, (_, destination, wide)): (&[u8], (u8, bool, bool)) =
        bits::<_, _, nom::error::Error<(&[u8], usize)>, _, _>(tuple((
            tag(0b100010, 6usize),
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
    let instr = if destination {
        Instr::Mov {
            dst: MovDest::Reg(reg),
            src: rm,
        }
    } else {
        Instr::Mov {
            dst: rm.try_into().unwrap(),
            src: RM::Reg(reg),
        }
    };
    Ok((input, instr))
}

/// Parse immediate to register instructions.
fn parse_mov_immediate(input: &[u8]) -> IResult<&[u8], Instr> {
    let (input, (_, wide, reg)): (&[u8], (u8, bool, u8)) =
        bits::<_, _, nom::error::Error<(&[u8], usize)>, _, _>(tuple((
            tag(0b1011, 4usize),
            map(take(1usize), |x: u8| x == 1),
            take(3usize),
        )))(input)?;

    let (input, imd) = if wide {
        parse_literal_wide(input)?
    } else {
        parse_literal_narrow(input)?
    };
    let instr = Instr::Mov {
        dst: MovDest::Reg(parse_register(wide, reg)),
        src: RM::Imd(imd),
    };
    Ok((input, instr))
}

fn parse_mov_immediate_to_reg_or_mem(input: &[u8]) -> IResult<&[u8], Instr> {
    // First byte
    let (input, (_, wide)): (&[u8], (u8, bool)) =
        bits::<_, _, nom::error::Error<(&[u8], usize)>, _, _>(tuple((
            tag(0b1100011, 7usize),
            map(take(1usize), |x: u8| x == 1),
        )))(input)?;

    // Second byte
    let (input, (mode, _, rm)): (&[u8], (MovMode, u8, u8)) =
        bits::<_, _, nom::error::Error<(&[u8], usize)>, _, _>(tuple((
            map(take(2usize), |code: u8| code.into()),
            tag(0b000, 3usize),
            take(3usize),
        )))(input)?;

    let (input, rm) = parse_rm(input, &mode, wide, rm)?;

    let (input, data) = if wide {
        let (input, literal) = parse_literal_wide(input)?;
        (input, literal)
    } else {
        let (input, literal) = parse_literal_narrow(input)?;
        (input, literal)
    };

    let instr = Instr::Mov {
        dst: rm.try_into().unwrap(),
        src: RM::Imd(data),
    };
    Ok((input, instr))
}

/// Helper for extracting similar accumulator-style MOVs.
fn parse_accum_style_mov(input: &[u8], code: u8) -> IResult<&[u8], Literal> {
    // First byte
    let (input, (_, wide)): (&[u8], (u8, bool)) =
        bits::<_, _, nom::error::Error<(&[u8], usize)>, _, _>(tuple((
            tag(code, 7usize),
            map(take(1usize), |x: u8| x == 1),
        )))(input)?;

    if wide {
        parse_literal_wide(input)
    } else {
        parse_literal_narrow(input)
    }
}

/// Parse memory to accumulator instructions.
fn parse_mov_mem_to_accum(input: &[u8]) -> IResult<&[u8], Instr> {
    let (input, data) = parse_accum_style_mov(input, 0b1010000)?;
    let mem_addr = MemAddr::new(None, None, Some(data));
    let instr = Instr::Mov {
        dst: MovDest::Reg(Register::AX),
        src: RM::Mem(mem_addr),
    };
    Ok((input, instr))
}

/// Parse accumulator to memory instructions.
fn parse_mov_accum_to_mem(input: &[u8]) -> IResult<&[u8], Instr> {
    let (input, data) = parse_accum_style_mov(input, 0b1010001)?;
    let mem_addr = MemAddr::new(None, None, Some(data));
    let instr = Instr::Mov {
        dst: MovDest::Mem(mem_addr),
        src: RM::Reg(Register::AX),
    };
    Ok((input, instr))
}

/// Parse the single next byte as a "literal."
fn parse_literal_narrow(input: &[u8]) -> IResult<&[u8], Literal> {
    map(take_bytes(1usize), |x: &[u8]| Literal::Narrow(x[0] as i8))(input)
}

/// Parse the next two bytes as a "literal."
fn parse_literal_wide(input: &[u8]) -> IResult<&[u8], Literal> {
    map(take_bytes(2usize), |x: &[u8]| {
        Literal::Wide((x[1] as u16) << 8 | (x[0] as u16))
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
                        let (input, literal) = parse_literal_wide(input)?;
                        (input, Some(literal))
                    } else {
                        (input, None)
                    }
                }
                Some(Displacement::D8) => {
                    let (input, literal) = parse_literal_narrow(input)?;
                    (input, Some(literal))
                }
                Some(Displacement::D16) => {
                    let (input, literal) = parse_literal_wide(input)?;
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
