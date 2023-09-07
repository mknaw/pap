use std::fmt::{self, Display};

#[derive(Clone, strum_macros::Display)]
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

#[derive(Clone, strum_macros::Display)]
#[strum(serialize_all = "lowercase")]
pub enum Segment {
    CS,
    DS,
    ES,
    SS,
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
                Literal::Byte(x) => write!(f, "{}{}", connector, x.abs())?,
                Literal::Word(x) => write!(f, "{}{}", connector, x)?,
            }
        };

        write!(f, "]")
    }
}

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
