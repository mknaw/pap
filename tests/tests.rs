use assert_cmd::Command;

#[test]
fn test_disassemble_listing_38() {
    let expected =
        std::str::from_utf8(std::fs::read("./assets/listing_38.asm").unwrap().as_slice())
            .unwrap()
            .lines()
            .filter(|line| line.starts_with("mov"))
            .into_iter()
            .collect::<Vec<_>>()
            .join("\n");
    Command::cargo_bin("pap")
        .unwrap()
        .arg("disassemble")
        .arg("./assets/listing_38")
        .assert()
        .success()
        .stdout(expected + "\n");
}
