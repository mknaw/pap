use std::fs::{read, File};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;

use tempfile::{tempdir, TempDir};

use pap::disassemble;

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

/// Assemble `target`, then run disassembler, assemble that, and compare the two assembled files.
/// Running these tests requires available `nasm` command!
fn test_disassemble(target: &Path) {
    let dir = tempdir().unwrap();
    let assembled = assemble(target, &dir);
    let data = std::fs::read(&assembled).unwrap();
    let input = data.as_slice();

    let disassembled = disassemble(input).unwrap_or_else(|e| panic!("{:?}", e));

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
        std::str::from_utf8(&read(target).unwrap()).unwrap(),
        std::fs::read_to_string(&disassembled_path).unwrap()
    );
}

#[test]
fn test_disassemble_listing_38() {
    test_disassemble(Path::new("./assets/listing_38.asm"));
}

#[test]
fn test_disassemble_listing_39() {
    test_disassemble(Path::new("./assets/listing_39.asm"));
}

#[test]
fn test_disassemble_listing_40() {
    test_disassemble(Path::new("./assets/listing_40.asm"));
}

#[test]
fn test_disassemble_listing_41() {
    test_disassemble(Path::new("./assets/listing_41.asm"));
}

#[test]
fn test_disassemble_listing_42() {
    test_disassemble(Path::new("./assets/listing_42.asm"));
}

#[test]
fn test_disassemble_listing_43() {
    test_disassemble(Path::new("./assets/listing_43.asm"));
}
