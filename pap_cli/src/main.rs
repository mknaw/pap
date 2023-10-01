use std::io::Write;
use std::path::PathBuf;

use clap::{Parser, Subcommand};
use pap::{disassemble, simulate};

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
    Simulate { path: PathBuf },
}

fn main() {
    let cli = Cli::parse();
    match &cli.command {
        Commands::Disassemble { path } => {
            let input = std::fs::read(path).unwrap();
            match disassemble(input.as_slice()) {
                Ok(instructions) => instructions.iter().for_each(|instr| println!("{}", instr)),
                Err(e) => {
                    std::io::stderr()
                        .write_all(e.to_string().as_bytes())
                        .unwrap();
                    std::process::exit(1);
                }
            }
        }
        Commands::Simulate { path } => {
            let input = std::fs::read(path).unwrap();
            match disassemble(input.as_slice()) {
                Ok(instructions) => {
                    let state = simulate(&instructions);
                    println!("{}", state);
                }
                Err(e) => {
                    std::io::stderr()
                        .write_all(e.to_string().as_bytes())
                        .unwrap();
                    std::process::exit(1);
                }
            }
        }
    }
}
