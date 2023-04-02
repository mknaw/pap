use std::io::Write;
use std::path::PathBuf;

use clap::{Parser, Subcommand};

pub(crate) mod parser;
pub(crate) mod utils;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Disassemble { path: PathBuf },
}

fn main() {
    let cli = Cli::parse();
    match &cli.command {
        Commands::Disassemble { path } => match parser::disassemble(path) {
            Ok(instructions) => instructions.iter().for_each(|instr| println!("{}", instr)),
            Err(e) => {
                std::io::stderr()
                    .write_all(e.to_string().as_bytes())
                    .unwrap();
                std::process::exit(1);
            }
        },
    }
}
