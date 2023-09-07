use std::fmt::{self, Display};

use crate::common::*;

pub struct State {
    ax: u16,
    bx: u16,
    cx: u16,
    dx: u16,
    sp: u16,
    bp: u16,
    si: u16,
    di: u16,
}

impl State {
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
        }
    }
}

impl Display for State {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "AX: 0x{:04X}", self.ax)?;
        writeln!(f, "BX: 0x{:04X}", self.bx)?;
        writeln!(f, "CX: 0x{:04X}", self.cx)?;
        writeln!(f, "DX: 0x{:04X}", self.dx)?;
        writeln!(f, "SP: 0x{:04X}", self.sp)?;
        writeln!(f, "BP: 0x{:04X}", self.bp)?;
        writeln!(f, "SI: 0x{:04X}", self.si)?;
        writeln!(f, "DI: 0x{:04X}", self.di)?;
        Ok(())
    }
}

pub fn simulate(instrs: &Vec<Instr>) -> State {
    let mut state = State::new();
    for instr in instrs {
        apply(&mut state, instr);
    }
    state
}

fn apply(state: &mut State, instr: &Instr) {
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

fn apply_mov(state: &mut State, dst: &RM, src: &RM) {
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
            _ => unimplemented!(),
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
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}
