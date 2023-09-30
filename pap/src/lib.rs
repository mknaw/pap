#[macro_use]
extern crate lazy_static;

pub mod common;
mod disassembler;
pub(crate) mod dsl;
mod simulator;

pub use common::Instr;
pub use disassembler::disassemble;
pub use simulator::{simulate, RegisterState};
