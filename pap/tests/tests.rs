use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;

use tempfile::tempdir;

use pap::disassemble;

// Running these tests requires available `nasm` command!
fn test_disassemble(target: &str) {
    let dir = tempdir().unwrap();
    let path = PathBuf::from(target);

    let disassembled = disassemble(&path).unwrap();

    let disassembled_path = dir.path().join("instructions.asm");
    let mut disassembled_file = File::create(&disassembled_path).unwrap();
    disassembled.iter().for_each(|instr| {
        writeln!(disassembled_file, "{}", instr).unwrap();
    });

    let status = Command::new("nasm")
        .arg(&disassembled_path)
        .status()
        .unwrap();
    assert!(status.success());
    let assembled = std::fs::read(dir.path().join("instructions")).unwrap();
    let original = std::fs::read(target).unwrap();
    assert_eq!(
        assembled,
        original,
        // "\n\nExpected:\n{}\n\nActual:\n{}",
        // print_vec_u8_bits(&original),
        // print_vec_u8_bits(&assembled)
    );
}

#[test]
fn test_disassemble_listing_38() {
    test_disassemble("./assets/listing_38");
}

#[test]
fn test_disassemble_listing_39() {
    test_disassemble("./assets/listing_39");
}

#[test]
fn test_disassemble_listing_40() {
    test_disassemble("./assets/listing_40");
}

#[test]
fn test_disassemble_listing_41() {
    test_disassemble("./assets/listing_41");
}

#[test]
fn test_disassemble_listing_42() {
    test_disassemble("./assets/listing_42");
}

#[test]
fn test_disassemble_listing_43() {
    test_disassemble("./assets/listing_43");
}
