use std::fmt::Display;
use std::path::PathBuf;

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

pub enum Immediate {
    Narrow(u8),
    Wide(u16),
}

pub enum MovSrc {
    Reg(Register),
    Mem(MemAddr),
    Imd(Immediate),
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

impl std::fmt::Display for Immediate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Narrow(x) => write!(f, "{}", x),
            Self::Wide(x) => write!(f, "{}", x),
        }
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
    let mut input = data.as_slice();
    let mut instructions = Vec::new();

    while !input.is_empty() {
        let (instruction, tail) = parse_instr(input)?;
        instructions.push(instruction);
        input = tail;
    }
    Ok(instructions)
}

#[derive(Debug)]
pub struct ParseError(Vec<u8>);

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Parse error @ {}", print_vec_u8_bits(&self.0))
    }
}

impl std::error::Error for ParseError {}

type ParseResult<'a> = Result<(Instr, &'a [u8]), ParseError>;

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

pub fn parse_instr(input: &[u8]) -> ParseResult {
    let first = input.first().ok_or(ParseError(input.to_vec()))?;
    if first >> 2 == 0b00100010 {
        parse_mov(input)
    } else if first >> 4 == 0b00001011 {
        parse_mov_immediate(input)
    } else {
        Err(ParseError(input.to_vec()))
    }
}

fn parse_mov(input: &[u8]) -> ParseResult {
    let (head, tail) = input.split_first().unwrap();
    let wide = head & 0b01 > 0;
    let destination = head & 0b10 > 0;
    let (head, tail) = tail.split_first().ok_or(ParseError(input.to_vec()))?;
    let reg = parse_register(wide, head >> 3 & 0b111);
    let rm = head & 0b111;
    let mode = (head >> 6).into();
    let (dst, src, tail) = match &mode {
        MovMode::MemMode(disp) => {
            let (disp, tail) = match disp {
                None => (0, tail),
                Some(Displacement::D8) => {
                    let (disp, tail) = tail.split_first().ok_or(ParseError(tail.to_vec()))?;
                    (*disp as u16, tail)
                }
                Some(Displacement::D16) => {
                    let (bottom, tail) = tail.split_first().ok_or(ParseError(tail.to_vec()))?;
                    let (top, tail) = tail.split_first().ok_or(ParseError(tail.to_vec()))?;
                    ((*top as u16) << 8 | (*bottom as u16), tail)
                }
            };

            let mem_addr = parse_effective_address(mode, rm, disp);
            if destination {
                (MovDest::Reg(reg), MovSrc::Mem(mem_addr), tail)
            } else {
                (MovDest::Mem(mem_addr), MovSrc::Reg(reg), tail)
            }
        }
        MovMode::RegMode => {
            let rm = parse_register(wide, rm);
            if destination {
                (MovDest::Reg(reg), MovSrc::Reg(rm), tail)
            } else {
                (MovDest::Reg(rm), MovSrc::Reg(reg), tail)
            }
        }
    };

    let instr = Instr::Mov { dst, src };
    Ok((instr, tail))
}

fn parse_effective_address(mode: MovMode, code: u8, disp: u16) -> MemAddr {
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

fn parse_mov_immediate(input: &[u8]) -> ParseResult {
    let (first, tail) = input.split_first().unwrap();
    let wide = first & 0b00001000 > 0;
    let reg = parse_register(wide, first & 0b111);
    let (second, tail) = tail.split_first().ok_or(ParseError(input.to_vec()))?;
    if wide {
        let (third, tail) = tail.split_first().ok_or(ParseError(input.to_vec()))?;
        let immediate = Immediate::Wide((*third as u16) << 8 | (*second as u16));
        let instr = Instr::Mov {
            dst: MovDest::Reg(reg),
            src: MovSrc::Imd(immediate),
        };
        Ok((instr, tail))
    } else {
        let immediate = Immediate::Narrow(*second);
        let instr = Instr::Mov {
            dst: MovDest::Reg(reg),
            src: MovSrc::Imd(immediate),
        };
        Ok((instr, tail))
    }
}
