use std::fmt::{self, Display};

use bitflags::bitflags;

use crate::common::*;

bitflags! {
    #[derive(Debug, Eq, PartialEq)]
    pub struct Flags: u16 {
        const O = 0b100000000000;
        const S = 0b000010000000;
        const Z = 0b000001000000;
        const A = 0b000000001000;
        const P = 0b000000000100;
        const C = 0b000000000001;
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct ProcessorState {
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
    pub flags: Flags,
}

impl ProcessorState {
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
            flags: Flags::empty(),
        }
    }

    fn set_zero(&mut self, value: u16) {
        if value == 0b0 {
            self.flags |= Flags::Z;
        } else {
            self.flags -= Flags::Z;
        }
    }

    fn set_sign(&mut self, value: u16) {
        if value & 0x8000 == 0 {
            self.flags -= Flags::S;
        } else {
            self.flags |= Flags::S;
        }
    }

    fn set_parity(&mut self, value: u16) {
        if (value & 0x00FF).count_ones() % 2 == 0 {
            self.flags |= Flags::P;
        } else {
            self.flags -= Flags::P;
        }
    }

    fn set_add_overflow(&mut self, lhs: u16, rhs: u16) {
        if lhs & 0x8000 == 0 && rhs & 0x8000 == 0 && ((lhs + rhs) & 0x8000) != 0 {
            self.flags |= Flags::O;
        } else {
            self.flags -= Flags::O;
        }
    }

    fn set_sub_overflow(&mut self, lhs: u16, rhs: u16) {
        if lhs & 0x8000 != 0 && rhs & 0x8000 == 0 && ((lhs - rhs) & 0x8000) == 0 {
            self.flags |= Flags::O;
        } else {
            self.flags -= Flags::O;
        }
    }

    fn set_aux_carry_sub(&mut self, lhs: u16, rhs: u16) {
        if rhs & 0x000F > lhs & 0x000F {
            self.flags |= Flags::A;
        } else {
            self.flags -= Flags::A;
        }
    }

    fn set_aux_carry_add(&mut self, lhs: u16, rhs: u16) {
        if ((rhs & 0x000F) + (lhs & 0x000F)) > 0x000F {
            self.flags |= Flags::A;
        } else {
            self.flags -= Flags::A;
        }
    }

    fn set_carry_add(&mut self, lhs: u16, rhs: u16) {
        if ((lhs & 0x7FFF) + (rhs & 0x7FFF) > 0x7FFF) && (lhs & 0x8000 == 0 && rhs & 0x8000 == 0) {
            self.flags |= Flags::C;
        } else {
            self.flags -= Flags::C;
        }
    }

    fn set_carry_sub(&mut self, lhs: u16, rhs: u16) {
        // TODO probably should try to implement this myself but tired rn
        if lhs.checked_sub(rhs).is_none() {
            self.flags |= Flags::C;
        } else {
            self.flags -= Flags::C;
        }
    }
}

impl Display for ProcessorState {
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

pub fn simulate(instrs: &Vec<Instr>) -> ProcessorState {
    let mut state = ProcessorState::new();
    for instr in instrs {
        apply(&mut state, instr);
    }
    state
}

fn apply(state: &mut ProcessorState, instr: &Instr) {
    match instr {
        Instr::BinaryInstr { op, dst, src } => match op {
            BinaryOp::Mov => {
                apply_mov(state, dst, src);
            }
            BinaryOp::Sub => {
                apply_sub(state, dst, src);
            }
            BinaryOp::Add => {
                apply_add(state, dst, src);
            }
            BinaryOp::Cmp => {
                apply_cmp(state, dst, src);
            }
            _ => unimplemented!(),
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

fn get_value(rm: &RM, state: &ProcessorState) -> u16 {
    match rm {
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
    }
}

fn apply_mov(state: &mut ProcessorState, dst: &RM, src: &RM) {
    let value = get_value(src, state);
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

fn set_standard_flags(state: &mut ProcessorState, val: u16) -> u16 {
    state.set_zero(val);
    state.set_parity(val);
    state.set_sign(val);
    val
}

fn set_sub_flags(state: &mut ProcessorState, lhs: u16, rhs: u16) -> u16 {
    let val = lhs.wrapping_sub(rhs);
    state.set_aux_carry_sub(lhs, rhs);
    state.set_sub_overflow(lhs, rhs);
    state.set_carry_sub(lhs, rhs);
    set_standard_flags(state, val)
}

fn set_add_flags(state: &mut ProcessorState, lhs: u16, rhs: u16) -> u16 {
    let val = lhs.wrapping_add(rhs);
    state.set_aux_carry_add(lhs, rhs);
    state.set_add_overflow(lhs, rhs);
    state.set_carry_add(lhs, rhs);
    set_standard_flags(state, val)
}

fn apply_sub(state: &mut ProcessorState, dst: &RM, src: &RM) {
    let value = get_value(src, state);
    match dst {
        RM::Reg(r) => match r {
            Register::AX => state.ax = set_sub_flags(state, state.ax, value),
            Register::BX => state.bx = set_sub_flags(state, state.bx, value),
            Register::CX => state.cx = set_sub_flags(state, state.cx, value),
            Register::DX => state.dx = set_sub_flags(state, state.dx, value),
            Register::SP => state.sp = set_sub_flags(state, state.sp, value),
            Register::BP => state.bp = set_sub_flags(state, state.bp, value),
            Register::SI => state.si = set_sub_flags(state, state.si, value),
            Register::DI => state.di = set_sub_flags(state, state.di, value),
            _ => todo!(),
        },
        RM::SegReg(sr) => match sr {
            Segment::CS => state.cs -= value,
            Segment::DS => state.ds -= value,
            Segment::ES => state.es -= value,
            Segment::SS => state.ss -= value,
        },
        _ => unimplemented!(),
    }
}

fn apply_add(state: &mut ProcessorState, dst: &RM, src: &RM) {
    let value = get_value(src, state);
    match dst {
        RM::Reg(r) => match r {
            Register::AX => state.ax = set_add_flags(state, state.ax, value),
            Register::BX => state.bx = set_add_flags(state, state.bx, value),
            Register::CX => state.cx = set_add_flags(state, state.cx, value),
            Register::DX => state.dx = set_add_flags(state, state.dx, value),
            Register::SP => state.sp = set_add_flags(state, state.sp, value),
            Register::BP => state.bp = set_add_flags(state, state.bp, value),
            Register::SI => state.si = set_add_flags(state, state.si, value),
            Register::DI => state.di = set_add_flags(state, state.di, value),
            _ => todo!(),
        },
        RM::SegReg(sr) => match sr {
            Segment::CS => state.cs += value,
            Segment::DS => state.ds += value,
            Segment::ES => state.es += value,
            Segment::SS => state.ss += value,
        },
        _ => unimplemented!(),
    }
}

fn apply_cmp(state: &mut ProcessorState, dst: &RM, src: &RM) {
    let value = get_value(src, state);
    match dst {
        RM::Reg(r) => match r {
            Register::AX => {
                set_sub_flags(state, state.ax, value);
            }
            Register::BX => {
                set_sub_flags(state, state.bx, value);
            }
            Register::CX => {
                set_sub_flags(state, state.cx, value);
            }
            Register::DX => {
                set_sub_flags(state, state.dx, value);
            }
            Register::SP => {
                set_sub_flags(state, state.sp, value);
            }
            Register::BP => {
                set_sub_flags(state, state.bp, value);
            }
            Register::SI => {
                set_sub_flags(state, state.si, value);
            }
            Register::DI => {
                set_sub_flags(state, state.di, value);
            }
            _ => todo!(),
        },
        RM::SegReg(sr) => match sr {
            Segment::CS => state.cs -= value,
            Segment::DS => state.ds -= value,
            Segment::ES => state.es -= value,
            Segment::SS => state.ss -= value,
        },
        _ => unimplemented!(),
    }
}
