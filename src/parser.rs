use std::path::PathBuf;

pub enum Instr {
    Mov {
        destination: bool,
        wide: bool,
        mode: Mode,
        reg: Register,
        rm: Register,
    },
}

impl std::fmt::Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instr::Mov {
                destination,
                reg,
                rm,
                ..
            } => {
                let src = if *destination { rm } else { reg };
                let dst = if *destination { reg } else { rm };
                write!(f, "mov {}, {}", dst, src)
            }
        }
    }
}

pub enum Mode {
    RegisterMode,
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
        let variant_name = format!("{:?}", self).to_lowercase();
        write!(f, "{}", variant_name)
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
        let (instruction, tail) = parse(input)?;
        instructions.push(instruction);
        input = tail;
    }
    Ok(instructions)
}

#[derive(Debug)]
pub struct ParseError;

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Parse error")
    }
}

impl std::error::Error for ParseError {}

pub fn parse(input: &[u8]) -> Result<(Instr, &[u8]), ParseError> {
    match input {
        [first, second, tail @ ..] => match first >> 2 {
            0b00100010 => Ok((parse_mov(&[*first, *second]), tail)),
            _ => Err(ParseError),
        },
        _ => Err(ParseError),
    }
}

fn parse_mov(bytes: &[u8; 2]) -> Instr {
    let [first, second] = bytes;
    let wide = first & 0b00000001 > 0;
    Instr::Mov {
        destination: first & 0b00000010 > 0,
        wide,
        mode: match second >> 6 {
            0b00000011 => Mode::RegisterMode,
            _ => panic!("Unknown mode!"),
        },
        reg: parse_register(wide, (second & 0b00111000) >> 3),
        rm: parse_register(wide, second & 0b00000111),
    }
}
