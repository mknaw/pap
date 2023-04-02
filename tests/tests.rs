use std::fs::File;
use std::io::Write;
use std::process::Command;

use assert_cmd::Command as AssertCommand;
use tempfile::tempdir;

// Running these tests requires available `nasm` command!
fn test_disassemble(target: &str) {
    let dir = tempdir().unwrap();

    let disassembled = AssertCommand::cargo_bin("pap")
        .unwrap()
        .arg("disassemble")
        .arg(target)
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();

    let disassembled_path = dir.path().join("instructions.asm");
    let mut disassembled_file = File::create(&disassembled_path).unwrap();
    disassembled_file.write_all(&disassembled).unwrap();

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
    // TODO
    test_disassemble("./assets/listing_39");
}
