use std::fs::{read, File};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;

use pap::{disassemble, simulate, Flags, Instr, ProcessorState};
use tempfile::{tempdir, TempDir};
use test_macros::generate_disassembler_tests;

/// Run `nasm` on the given file and return the path to the assembled file.
fn assemble(path: &Path, dir: &TempDir) -> PathBuf {
    let out_path = dir.path().join("out");
    let status = Command::new("nasm")
        .args([path.to_str().unwrap(), "-o", out_path.to_str().unwrap()])
        .status()
        .unwrap();
    assert!(status.success());
    out_path
}

/// Pretty-print `bytes`.
fn format_octets(bytes: &[u8]) -> String {
    bytes
        .iter()
        .map(|b| format!("{:08b}", b))
        .collect::<Vec<_>>()
        .join(" ")
}

fn parse_instrs(path: &PathBuf) -> (TempDir, PathBuf, Vec<Instr>) {
    let path = Path::new(path);
    let dir = tempdir().unwrap();
    let assembled = assemble(path, &dir);
    let data = std::fs::read(&assembled).unwrap();
    let input = data.as_slice();
    let instrs = disassemble(input).unwrap_or_else(|e| panic!("{:?}", e));
    (dir, assembled, instrs)
}

/// Assemble `target`, then run disassembler, assemble that, and compare the two assembled files.
/// Running these tests requires available `nasm` command!
fn test_disassemble(target: &PathBuf) {
    let (dir, assembled, disassembled) = parse_instrs(target);

    let disassembled_path = dir.path().join("instructions.asm");
    let mut disassembled_file = File::create(&disassembled_path).unwrap();
    disassembled.iter().for_each(|instr| {
        writeln!(disassembled_file, "{}", instr).unwrap();
    });

    let status = Command::new("nasm")
        .arg(&disassembled_path)
        .status()
        .unwrap();
    assert!(
        status.success(),
        "nasm assembly failed:\n\n{}",
        disassembled
            .iter()
            .map(|i| format!("{}", i))
            .collect::<Vec<_>>()
            .join("\n")
    );
    let reassembled = std::fs::read(dir.path().join("instructions")).unwrap();
    let original = std::fs::read(&assembled).unwrap();
    assert_eq!(
        format_octets(&original),
        format_octets(&reassembled),
        "\n\nExpected:\n{}//\n\nActual:\n{}",
        std::str::from_utf8(&read(Path::new(target)).unwrap()).unwrap(),
        std::fs::read_to_string(&disassembled_path).unwrap()
    );
}

// Disassembly tests

generate_disassembler_tests!();

// Simulator tests

#[test]
fn simulate_listing_43() {
    let (_, _, instrs) = parse_instrs(&Path::new("./tests/cases/listing_43.asm").to_path_buf());
    let state = simulate(&instrs);
    assert_eq!(
        state,
        ProcessorState {
            ax: 1,
            bx: 2,
            cx: 3,
            dx: 4,
            sp: 5,
            bp: 6,
            si: 7,
            di: 8,
            cs: 0,
            ds: 0,
            es: 0,
            ss: 0,
            flags: Flags::empty(),
        }
    )
}

#[test]
fn simulate_listing_44() {
    let (_, _, instrs) = parse_instrs(&Path::new("./tests/cases/listing_44.asm").to_path_buf());
    let state = simulate(&instrs);
    assert_eq!(
        state,
        ProcessorState {
            ax: 4,
            bx: 3,
            cx: 2,
            dx: 1,
            sp: 1,
            bp: 2,
            si: 3,
            di: 4,
            cs: 0,
            ds: 0,
            es: 0,
            ss: 0,
            flags: Flags::empty(),
        }
    )
}

#[test]
fn simulate_listing_45() {
    let (_, _, instrs) = parse_instrs(&Path::new("./tests/cases/listing_45.asm").to_path_buf());
    let state = simulate(&instrs);
    assert_eq!(
        state,
        ProcessorState {
            ax: 0x4411,
            bx: 0x3344,
            cx: 0x6677,
            dx: 0x7788,
            sp: 0x4411,
            bp: 0x3344,
            si: 0x6677,
            di: 0x7788,
            cs: 0,
            es: 0x6677,
            ss: 0x4411,
            ds: 0x3344,
            flags: Flags::empty(),
        }
    )
}

#[test]
fn simulate_listing_46() {
    let (_, _, instrs) = parse_instrs(&Path::new("./tests/cases/listing_46.asm").to_path_buf());
    let state = simulate(&instrs);
    assert_eq!(
        state,
        ProcessorState {
            ax: 0,
            bx: 0xE102,
            cx: 0x0F01,
            dx: 0,
            sp: 0x03E6,
            bp: 0,
            si: 0,
            di: 0,
            cs: 0,
            es: 0,
            ss: 0,
            ds: 0,
            flags: Flags::P | Flags::Z,
        }
    )
}
