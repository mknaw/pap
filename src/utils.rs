use itertools::Itertools;

pub fn print_u8_bits(x: &u8) -> String {
    (0..8).rev().fold(String::new(), |acc, i| {
        acc + if x & (1 << i) != 0 { "1" } else { "0" }
    })
}

pub fn print_vec_u8_bits(v: &Vec<u8>) -> String {
    v.into_iter()
        .chunks(4)
        .into_iter()
        .map(|chunk| chunk.map(print_u8_bits).collect::<Vec<_>>().join(" "))
        .collect::<Vec<_>>()
        .join("\n")
}

#[cfg(test)]
mod tests {
    use super::{print_u8_bits, print_vec_u8_bits};

    #[test]
    fn test_print_u8_bits() {
        assert_eq!(print_u8_bits(&0b10001001), "10001001");
    }

    #[test]
    fn test_print_vec_u8_bits() {
        let vec = vec![0b10001001, 0b10111000, 0b00110011, 0b11110000, 0b00001111];
        assert_eq!(
            print_vec_u8_bits(&vec),
            "10001001 10111000 00110011 11110000\n00001111"
        );
    }
}
