use std::fmt::Display;
use std::path::PathBuf;

use nom::bits::bits;
use nom::bits::complete::{tag, take};
use nom::branch::alt;
use nom::bytes::complete::take as take_bytes;
use nom::combinator::{iterator, map};
use nom::sequence::tuple;
use nom::IResult;

use crate::utils::print_vec_u8_bits;

pub enum MovDest {
    Reg(Register),
    Mem(MemAddr),
}

impl Display for MovDest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MovDest::Reg(reg) => write!(f, "{}", reg),
            MovDest::Mem(ea) => write!(f, "{}", ea),
        }
    }
}

pub enum MovSrc {
    Reg(Register),
    Mem(MemAddr),
    Imd(u16),
}

impl Display for MovSrc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MovSrc::Reg(reg) => write!(f, "{}", reg),
            MovSrc::Mem(ea) => write!(f, "{}", ea),
            MovSrc::Imd(imd) => write!(f, "{}", imd),
        }
    }
}

pub enum Instr {
    Mov { dst: MovDest, src: MovSrc },
}

impl std::fmt::Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

impl std::fmt::Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", format!("{:?}", self).to_lowercase())
    }
}

pub struct MemAddr {
    reg1: Option<Register>,
    reg2: Option<Register>,
    disp: u16,
}

impl std::fmt::Display for MemAddr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut vec: Vec<String> = vec![&self.reg1, &self.reg2]
            .into_iter()
            .filter_map(|opt| opt.as_ref().map(|val| val.to_string()))
            .collect();
        if self.disp != 0 {
            vec.push(self.disp.to_string());
        }
        write!(f, "[{}]", vec.join(" + "))
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
    it.finish().unwrap();
    Ok(instrs)
}

#[derive(Debug)]
pub struct ParseError(Vec<u8>);

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
    alt((parse_mov, parse_mov_immediate))(input)
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

    let (input, dst, src) = match &mode {
        MovMode::MemMode(disp) => {
            let (input, disp) = match disp {
                None => (input, 0),
                Some(Displacement::D8) => parse_imd_narrow(input)?,
                Some(Displacement::D16) => parse_imd_wide(input)?,
            };
            let mem_addr = parse_effective_address(&mode, rm, disp);
            if destination {
                (input, MovDest::Reg(reg), MovSrc::Mem(mem_addr))
            } else {
                (input, MovDest::Mem(mem_addr), MovSrc::Reg(reg))
            }
        }
        MovMode::RegMode => {
            let rm = parse_register(wide, rm);
            if destination {
                (input, MovDest::Reg(reg), MovSrc::Reg(rm))
            } else {
                (input, MovDest::Reg(rm), MovSrc::Reg(reg))
            }
        }
    };

    Ok((input, Instr::Mov { dst, src }))
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
        parse_imd_wide(input)?
    } else {
        parse_imd_narrow(input)?
    };
    let instr = Instr::Mov {
        dst: MovDest::Reg(parse_register(wide, reg)),
        src: MovSrc::Imd(imd),
    };
    Ok((input, instr))
}

/// Parse the single next byte as a "literal."
fn parse_imd_narrow(input: &[u8]) -> IResult<&[u8], u16> {
    map(take_bytes(1usize), |x: &[u8]| x[0] as u16)(input)
}

/// Parse the next two bytes as a "literal."
fn parse_imd_wide(input: &[u8]) -> IResult<&[u8], u16> {
    map(take_bytes(2usize), |x: &[u8]| {
        (x[1] as u16) << 8 | (x[0] as u16)
    })(input)
}

fn parse_effective_address(mode: &MovMode, code: u8, disp: u16) -> MemAddr {
    let (reg1, reg2) = match code {
        0b000 => (Some(Register::BX), Some(Register::SI)),
        0b001 => (Some(Register::BX), Some(Register::DI)),
        0b010 => (Some(Register::BP), Some(Register::SI)),
        0b011 => (Some(Register::BP), Some(Register::DI)),
        0b100 => (Some(Register::SI), None),
        0b101 => (Some(Register::DI), None),
        0b110 => match mode {
            // TODO
            MovMode::MemMode(None) => panic!("TODO not sure how I'll do this one yet"),
            _ => (Some(Register::BP), None),
        },
        0b111 => (Some(Register::BX), None),
        _ => panic!("invalid input code"),
    };
    MemAddr { reg1, reg2, disp }
}
