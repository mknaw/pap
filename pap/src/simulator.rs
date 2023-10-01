use std::fmt::{self, Display};

use crate::common::*;

#[derive(Debug, Eq, PartialEq)]
pub struct RegisterState {
    pub ax: u16,
    pub bx: u16,
    pub cx: u16,
    pub dx: u16,
    pub sp: u16,
    pub bp: u16,
    pub si: u16,
    pub di: u16,
    pub cs: u16,
    pub ds: u16,
    pub es: u16,
    pub ss: u16,
}

impl RegisterState {
    fn new() -> Self {
        Self {
            ax: 0,
            bx: 0,
            cx: 0,
            dx: 0,
            sp: 0,
            bp: 0,
            si: 0,
            di: 0,
            cs: 0,
            ds: 0,
            es: 0,
            ss: 0,
        }
    }
}

impl Display for RegisterState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "AX: 0x{:04X}", self.ax)?;
        writeln!(f, "BX: 0x{:04X}", self.bx)?;
        writeln!(f, "CX: 0x{:04X}", self.cx)?;
        writeln!(f, "DX: 0x{:04X}", self.dx)?;
        writeln!(f, "SP: 0x{:04X}", self.sp)?;
        writeln!(f, "BP: 0x{:04X}", self.bp)?;
        writeln!(f, "SI: 0x{:04X}", self.si)?;
        writeln!(f, "DI: 0x{:04X}", self.di)?;
        writeln!(f, "CS: 0x{:04X}", self.cs)?;
        writeln!(f, "DS: 0x{:04X}", self.ds)?;
        writeln!(f, "ES: 0x{:04X}", self.es)?;
        writeln!(f, "SS: 0x{:04X}", self.ss)?;
        Ok(())
    }
}

pub fn simulate(instrs: &Vec<Instr>) -> RegisterState {
    let mut state = RegisterState::new();
    for instr in instrs {
        apply(&mut state, instr);
    }
    state
}

fn apply(state: &mut RegisterState, instr: &Instr) {
    match instr {
        Instr::BinaryInstr { op, dst, src } => match op {
            BinaryOp::Mov => {
                apply_mov(state, dst, src);
            }
            _ => {
                unimplemented!()
            }
        },
        _ => unimplemented!(),
    }
}

fn move_to_lower_byte(dst: u16, src: u8) -> u16 {
    let dst = dst & 0xFF00;
    let src = src as u16;
    dst | src
}

fn move_to_higher_byte(dst: u16, src: u8) -> u16 {
    let dst = dst & 0x00FF;
    let src = (src as u16) << 8;
    dst | src
}

fn apply_mov(state: &mut RegisterState, dst: &RM, src: &RM) {
    let value = match src {
        RM::Reg(r) => match r {
            Register::AX => state.ax,
            Register::BX => state.bx,
            Register::CX => state.cx,
            Register::DX => state.dx,
            Register::SP => state.sp,
            Register::BP => state.bp,
            Register::SI => state.si,
            Register::DI => state.di,
            // TODO wonder whether I shouldn't convert these to u8s here...
            Register::AL => state.ax & 0x00FF,
            Register::BL => state.bx & 0x00FF,
            Register::CL => state.cx & 0x00FF,
            Register::DL => state.dx & 0x00FF,
            Register::AH => (state.ax & 0xFF00) >> 8,
            Register::BH => (state.bx & 0xFF00) >> 8,
            Register::CH => (state.cx & 0xFF00) >> 8,
            Register::DH => (state.dx & 0xFF00) >> 8,
        },
        RM::SegReg(sr) => match sr {
            Segment::CS => state.cs,
            Segment::DS => state.ds,
            Segment::ES => state.es,
            Segment::SS => state.ss,
        },
        RM::Imd(lit) => match lit {
            Literal::Byte(b) => *b as u16,
            Literal::Word(w) => *w,
        },
        _ => unimplemented!(),
    };
    match dst {
        RM::Reg(r) => match r {
            Register::AX => state.ax = value,
            Register::BX => state.bx = value,
            Register::CX => state.cx = value,
            Register::DX => state.dx = value,
            Register::SP => state.sp = value,
            Register::BP => state.bp = value,
            Register::SI => state.si = value,
            Register::DI => state.di = value,
            Register::AL => state.ax = move_to_lower_byte(state.ax, value as u8),
            Register::BL => state.bx = move_to_lower_byte(state.bx, value as u8),
            Register::CL => state.cx = move_to_lower_byte(state.cx, value as u8),
            Register::DL => state.dx = move_to_lower_byte(state.dx, value as u8),
            Register::AH => state.ax = move_to_higher_byte(state.ax, value as u8),
            Register::BH => state.bx = move_to_higher_byte(state.bx, value as u8),
            Register::CH => state.cx = move_to_higher_byte(state.cx, value as u8),
            Register::DH => state.dx = move_to_higher_byte(state.dx, value as u8),
        },
        RM::SegReg(sr) => match sr {
            Segment::CS => state.cs = value,
            Segment::DS => state.ds = value,
            Segment::ES => state.es = value,
            Segment::SS => state.ss = value,
        },
        _ => unimplemented!(),
    }
}
