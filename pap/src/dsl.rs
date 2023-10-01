use std::iter;

use bitvec::prelude::*;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alphanumeric1, digit1, line_ending, multispace0};
use nom::combinator::map;
use nom::multi::{many0, separated_list1};
use nom::IResult;

// TODO name
#[derive(Debug, PartialEq)]
pub struct Spec {
    pub mnemonic: String,
    pub code: BitVec<u8, Msb0>,
    pub rest: Vec<InstructionPart>,
}

impl Spec {
    // TODO have a less alloc-y version?
    pub fn iterate_parts(&self) -> impl Iterator<Item = InstructionPart> + '_ {
        iter::once(InstructionPart::Literal(self.code.clone())).chain(self.rest.iter().cloned())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum InstructionPart {
    Literal(BitVec<u8, Msb0>), // TODO should be a BitBox I reckon
    REG,
    RM,
    // Seg(BitArray<[u8; 1], Msb0>),
    MOD,
    SR,
    W,
    S,
    D,
    V,
    Z,
    START,
    END,
    OFFSETW,
    OFFSET,
}

fn parse_bit_literal(input: &str) -> IResult<&str, BitVec<u8, Msb0>> {
    let (input, bit_string) = digit1(input)?;
    let bits = bit_string.chars().map(|c| c == '1').collect::<_>();
    Ok((input, bits))
}

fn parse_instruction_part(input: &str) -> IResult<&str, InstructionPart> {
    let (input, _) = multispace0(input)?;
    alt((
        map(parse_bit_literal, InstructionPart::Literal),
        map(tag("REG"), |_| InstructionPart::REG),
        map(tag("RM"), |_| InstructionPart::RM),
        map(tag("MOD"), |_| InstructionPart::MOD),
        map(tag("SR"), |_| InstructionPart::SR),
        // TODO START/END could really be a single tag
        map(tag("START"), |_| InstructionPart::START),
        map(tag("END"), |_| InstructionPart::END),
        map(tag("OFFSETW"), |_| InstructionPart::OFFSETW),
        map(tag("OFFSET"), |_| InstructionPart::OFFSET),
        map(tag("W"), |_| InstructionPart::W),
        map(tag("S"), |_| InstructionPart::S),
        map(tag("D"), |_| InstructionPart::D),
        map(tag("V"), |_| InstructionPart::V),
        map(tag("Z"), |_| InstructionPart::Z),
    ))(input)
}

fn parse_line(input: &str) -> IResult<&str, Spec> {
    // TODO I think some of the instr codes have numbers
    let (input, mnemonic) = map(alphanumeric1, |s: &str| s.to_string())(input)?;
    let (input, _) = multispace0(input)?;
    let (input, code) = parse_bit_literal(input)?;
    let (input, rest) = many0(parse_instruction_part)(input)?;
    Ok((
        input,
        Spec {
            mnemonic,
            code,
            rest,
        },
    ))
}

// TODO probably want to convert this to a HashMap keyed on the `code`
pub fn construct_instruction_set() -> Vec<Spec> {
    let input = include_str!("../assets/8086.instr");
    let (_, specs) = separated_list1(line_ending, parse_line)(&input).unwrap();
    specs
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_line() {
        let (_, spec) = parse_line("PUSH 1111111 W MOD 110 RM").unwrap();
        assert_eq!(
            spec,
            Spec {
                mnemonic: "PUSH".to_string(),
                code: bitvec![u8, Msb0; 1, 1, 1, 1, 1, 1, 1],
                rest: vec![
                    InstructionPart::W,
                    InstructionPart::MOD,
                    InstructionPart::Literal(bitvec![u8, Msb0; 1, 1, 0]),
                    InstructionPart::RM,
                ],
            }
        );
    }

    #[test]
    fn test_construct_instruction_set() {
        let specs = construct_instruction_set();
        let xlat = Spec {
            mnemonic: "xlat".to_string(),
            code: bitvec![u8, Msb0; 1, 1, 0, 1, 0, 1, 1, 1],
            rest: vec![],
        };
        assert!(specs.iter().any(|spec| spec == &xlat));
    }
}
