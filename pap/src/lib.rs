#[macro_use]
extern crate lazy_static;

pub mod common;
mod disassembler;
mod simulator;
pub(crate) mod utils;

pub use disassembler::disassemble;
pub use simulator::simulate;
