use std::collections::HashMap;
use std::str::FromStr;

use anyhow::anyhow;
use bitvec::prelude::*;

use crate::common::*;
use crate::dsl::{construct_instruction_set, InstructionPart, Spec};

type BitV = BitVec<u8, Msb0>;
type BitS = BitSlice<u8, Msb0>;
type SpecMap<'a> = HashMap<BitV, &'a Spec>;

lazy_static! {
    static ref SPECS: Vec<Spec> = construct_instruction_set();
    // Could probably write a serialized version of `SPEC_MAP` to disk and load
    // at runtime instead of doing the combinatorics, but the combinatorics seem
    // fast enough for the purposes of this exercise.
    static ref SPEC_MAP: SpecMap<'static> = construct_spec_map(&SPECS);
    static ref CODE_TO_REGISTER: HashMap<u8, Register> = HashMap::from([
        (0b000, Register::AL),
        (0b001, Register::CL),
        (0b010, Register::DL),
        (0b011, Register::BL),
        (0b100, Register::AH),
        (0b101, Register::CH),
        (0b110, Register::DH),
        (0b111, Register::BH),
    ]);
    static ref CODE_TO_REGISTER_WIDE: HashMap<u8, Register> = HashMap::from([
        (0b000, Register::AX),
        (0b001, Register::CX),
        (0b010, Register::DX),
        (0b011, Register::BX),
        (0b100, Register::SP),
        (0b101, Register::BP),
        (0b110, Register::SI),
        (0b111, Register::DI),
    ]);
    static ref CODE_TO_MOD_MODE: HashMap<u8, MovMode> = HashMap::from([
        (0b00, MovMode::MemMode(None)),
        (0b01, MovMode::MemMode(Some(Displacement::D8))),
        (0b10, MovMode::MemMode(Some(Displacement::D16))),
        (0b11, MovMode::RegMode),
    ]);
    static ref CODE_TO_SEGMENT: HashMap<u8, Segment> = HashMap::from([
        (0b00, Segment::ES),
        (0b01, Segment::CS),
        (0b10, Segment::SS),
        (0b11, Segment::DS),
    ]);
}

trait OptionZip {
    type Output;

    fn zip(self) -> Option<Self::Output>;
}

impl<A, B, C> OptionZip for (Option<A>, Option<B>, Option<C>) {
    type Output = (A, B, C);

    fn zip(self) -> Option<Self::Output> {
        match self {
            (Some(a), Some(b), Some(c)) => Some((a, b, c)),
            _ => None,
        }
    }
}

impl<A, B, C, D, E> OptionZip for (Option<A>, Option<B>, Option<C>, Option<D>, Option<E>) {
    type Output = (A, B, C, D, E);

    fn zip(self) -> Option<Self::Output> {
        match self {
            (Some(a), Some(b), Some(c), Some(d), Some(e)) => Some((a, b, c, d, e)),
            _ => None,
        }
    }
}

#[derive(Debug)]
struct IntermediateInstr {
    mnemonic: String,
    code: BitV,
    w: Option<bool>,
    s: Option<bool>,
    d: Option<bool>,
    v: Option<bool>,
    z: Option<bool>,
    reg: Option<u8>,
    rm: Option<u8>,
    mm: Option<u8>,
    sr: Option<u8>,
    start: Option<Literal>,
    end: Option<Literal>,
    offset: Option<Offset>,
}

#[derive(Debug)]
enum BitSchemaPart {
    Fixed(BitV),     // Fixed value with a number of bits
    Variable(usize), // Variable value with a bit length
}

// TODO there should be no need to first allocate to a `Vec` just to move it to `HashMap`
fn generate_combinations(bit_schema: &[BitSchemaPart]) -> Vec<BitV> {
    let mut combinations = vec![BitVec::new()];

    for schema in bit_schema {
        let new_combinations = combinations
            .iter()
            .flat_map(|combo| match schema {
                BitSchemaPart::Fixed(value) => {
                    let mut value = value.clone();
                    let mut combo = combo.clone();
                    combo.append(&mut value);
                    vec![combo]
                }
                BitSchemaPart::Variable(length) => (0..(1 << length))
                    .map(|i| {
                        let mut new_combo = combo.clone();
                        for j in 0..*length {
                            new_combo.push((i & (1 << j)) != 0);
                        }
                        new_combo
                    })
                    .collect::<Vec<_>>(),
            })
            .collect();

        combinations = new_combinations;
    }
    combinations
}

fn spec_to_bit_schema(spec: &Spec) -> Vec<BitSchemaPart> {
    let mut schema = vec![BitSchemaPart::Fixed(spec.code.clone())];
    for part in spec.rest.iter() {
        if let Some(part) = match part {
            InstructionPart::W
            | InstructionPart::S
            | InstructionPart::D
            | InstructionPart::V
            | InstructionPart::Z => Some(BitSchemaPart::Variable(1)),
            InstructionPart::MOD | InstructionPart::SR => Some(BitSchemaPart::Variable(2)),
            InstructionPart::REG | InstructionPart::RM => Some(BitSchemaPart::Variable(3)),
            InstructionPart::Literal(bits) => Some(BitSchemaPart::Fixed(bits.clone())),
            _ => None,
        } {
            schema.push(part);
        }
    }
    schema
}

fn construct_spec_map(specs: &[Spec]) -> SpecMap {
    let mut spec_map = HashMap::new();
    for spec in specs {
        let bit_schemas = spec_to_bit_schema(spec);
        for combo in generate_combinations(&bit_schemas) {
            assert!(
                combo.len() == 8 || combo.len() == 16,
                "{:?} {:?}",
                spec,
                combo
            );
            spec_map.insert(combo, spec);
        }
    }
    spec_map
}

pub fn disassemble(input: &[u8]) -> anyhow::Result<Vec<Instr>> {
    let mut parser = Disassembler::new(input.view_bits::<Msb0>());
    parser.disassemble()
}

struct Disassembler<'a> {
    bits: &'a BitS,
    modifier: Option<ModifierOp>,
    segment: Option<Segment>,
}

impl<'a> Disassembler<'a> {
    pub fn new(bits: &'a BitS) -> Self {
        Self {
            bits,
            modifier: None,
            segment: None,
        }
    }

    fn is_finished(&self) -> bool {
        self.bits.is_empty()
    }

    pub fn disassemble(&mut self) -> anyhow::Result<Vec<Instr>> {
        let mut result = Vec::new();
        while !self.is_finished() {
            if let Some(instr) = self.disassemble_one()? {
                match self.modifier {
                    Some(ModifierOp::Rep(z)) => {
                        result.push(Instr::RepInstr {
                            instr: Box::new(instr),
                            zero_flag: z,
                        });
                    }
                    Some(ModifierOp::Lock) => {
                        result.push(Instr::LockInstr {
                            instr: Box::new(instr),
                        });
                    }
                    _ => {
                        result.push(instr);
                    }
                }
                self.modifier = None;
            }
        }

        if self.is_finished() {
            Ok(result)
        } else {
            Err(anyhow!("not all bytes were parsed!"))
        }
    }

    fn disassemble_one(&mut self) -> anyhow::Result<Option<Instr>> {
        if let Some(segment) = self.try_segment()? {
            if self.segment.is_none() {
                self.segment = Some(segment);
                return Ok(None);
            } else {
                anyhow::bail!("unexpected doubled segment set");
            }
        }

        let spec = SPEC_MAP
            .get(&self.bits[..8].to_bitvec())
            .or_else(|| SPEC_MAP.get(&self.bits[..16].to_bitvec()))
            .ok_or_else(|| anyhow!("no matching spec: {:08b}", self.bits[..8].load::<u8>()))?;

        if let Some(intermediate) = self.try_spec(spec)? {
            return self.intermediate_to_instr(intermediate);
        }

        anyhow::bail!("no matching spec: {:08b}", self.bits[..8].load::<u8>())
    }

    fn try_segment(&mut self) -> anyhow::Result<Option<Segment>> {
        let (head, bits) = self.bits.split_at(3);
        if head.load::<u8>() != 0b001 {
            return Ok(None);
        }
        let (sr, bits) = bits.split_at(2);
        let (head, bits) = bits.split_at(3);
        if head.load::<u8>() != 0b110 {
            return Ok(None);
        }
        let sr = sr.load();
        self.bits = bits;
        CODE_TO_SEGMENT
            .get(&sr)
            .copied()
            .map_or(Err(anyhow!("Unexpected segment code {:?}", sr)), |seg| {
                Ok(Some(seg))
            })
    }

    fn try_spec(&mut self, spec: &Spec) -> anyhow::Result<Option<IntermediateInstr>> {
        let mut w = None;
        let mut s = None;
        let mut d = None;
        let mut v = None;
        let mut z = None;
        let mut reg = None;
        let mut rm = None;
        let mut mm = None;
        let mut sr = None;
        let mut start = None;
        let mut end = None;
        let mut offset = None;

        macro_rules! split_bits {
            ($var:expr, $n:expr) => {{
                if self.bits.len() < $n {
                    return Ok(None);
                }
                let (head, tail) = self.bits.split_at($n);
                self.bits = tail;
                $var = Some(head.to_bitvec())
            }};
        }

        for part in spec.iterate_parts() {
            match part {
                InstructionPart::Literal(lit) => {
                    if self.bits.len() < lit.len() {
                        return Ok(None);
                    }
                    let (head, tail) = self.bits.split_at(lit.len());
                    self.bits = tail;
                    if head != lit {
                        return Ok(None);
                    }
                }
                InstructionPart::W => split_bits!(w, 1),
                InstructionPart::S => split_bits!(s, 1),
                InstructionPart::D => split_bits!(d, 1),
                InstructionPart::V => split_bits!(v, 1),
                InstructionPart::Z => split_bits!(z, 1),
                InstructionPart::REG => split_bits!(reg, 3),
                InstructionPart::RM => split_bits!(rm, 3),
                InstructionPart::MOD => split_bits!(mm, 2),
                InstructionPart::SR => split_bits!(sr, 2),
                InstructionPart::START => {
                    let literal = self.parse_literal(true)?;
                    start = Some(literal);
                }
                InstructionPart::END => {
                    let literal = self.parse_literal(true)?;
                    end = Some(literal);
                }
                InstructionPart::OFFSET => {
                    let (tail, literal) = parse_offset_byte(self.bits);
                    self.bits = tail;
                    offset = Some(literal);
                }
                InstructionPart::OFFSETW => {
                    let (tail, literal) = parse_offset_word(self.bits);
                    self.bits = tail;
                    offset = Some(literal);
                }
            }
        }
        Ok(Some(IntermediateInstr {
            mnemonic: spec.mnemonic.clone(),
            code: spec.code.clone(),
            w: w.map(|b| b[0]),
            s: s.map(|b| b[0]),
            d: d.map(|b| b[0]),
            v: v.map(|b| b[0]),
            z: z.map(|b| b[0]),
            reg: reg.map(|b| b.load::<u8>()),
            rm: rm.map(|b| b.load::<u8>()),
            mm: mm.map(|b| b.load::<u8>()),
            sr: sr.map(|b| b.load::<u8>()),
            start,
            end,
            offset,
        }))
    }

    fn parse_rm(&mut self, rm: u8, mm: u8, wide: Option<bool>) -> anyhow::Result<RM> {
        let mode = &parse_mod_mode(mm)?;
        let rm = match mode {
            MovMode::MemMode(disp) => {
                let (rest, disp) = match disp {
                    None => {
                        if rm == 0b110 {
                            let literal = self.parse_literal(true)?;
                            (self.bits, Some(literal))
                        } else {
                            (self.bits, None)
                        }
                    }
                    Some(Displacement::D8) => {
                        let literal = self.parse_literal(false)?;
                        (self.bits, Some(literal))
                    }
                    Some(Displacement::D16) => {
                        let literal = self.parse_literal(true)?;
                        (self.bits, Some(literal))
                    }
                };
                let mem_addr = parse_effective_address(mode, rm, disp, self.segment)?;
                self.bits = rest;
                Ok(RM::Mem(mem_addr))
            }
            MovMode::RegMode => {
                let w = wide.unwrap_or(true);
                let register = parse_register(rm, w)?;
                Ok(RM::Reg(register))
            }
        };
        self.segment = None;
        rm
    }

    fn parse_literal(&mut self, wide: bool) -> anyhow::Result<Literal> {
        if wide {
            let (rest, literal) = parse_literal_word(self.bits);
            self.bits = rest;
            Ok(literal)
        } else {
            let (rest, literal) = parse_literal_byte(self.bits);
            self.bits = rest;
            Ok(literal)
        }
    }

    fn parse_byte_offset(&mut self) -> anyhow::Result<Offset> {
        let (inc, rest) = self.bits.split_at(8);
        self.bits = rest;
        Ok(Offset::Byte(inc.load::<i8>()))
    }

    fn intermediate_to_instr(
        &mut self,
        intermediate: IntermediateInstr,
    ) -> anyhow::Result<Option<Instr>> {
        if let Ok(op) = NullaryOp::from_str(&intermediate.mnemonic) {
            Ok(Some(Instr::NullaryInstr { op }))
        } else if let Ok(op) = UnaryOp::from_str(&intermediate.mnemonic) {
            Ok(Some(self.disassemble_unary_op(op, &intermediate)?))
        } else if let Ok(op) = BinaryOp::from_str(&intermediate.mnemonic) {
            Ok(Some(self.disassemble_binary_op(op, &intermediate)?))
        } else if let Ok(op) = JumpOp::from_str(&intermediate.mnemonic) {
            let offset = self.parse_byte_offset()?;
            let instr = Instr::JumpInstr { op, offset };
            Ok(Some(instr))
        } else if let Some((op, rm, mm, v, w)) = (
            ShiftRotOp::from_str(&intermediate.mnemonic).ok(),
            intermediate.rm,
            intermediate.mm,
            intermediate.v,
            intermediate.w,
        )
            .zip()
        {
            let rm = self.parse_rm(rm, mm, Some(w))?;
            let src = if v {
                ShiftRotSrc::RM(RM::Reg(Register::CL))
            } else {
                ShiftRotSrc::One
            };
            let instr = Instr::ShiftRotInstr {
                op,
                dst: rm,
                src,
                wide: w,
            };
            Ok(Some(instr))
        } else if let Ok(op) = ModifierOp::from_str(&intermediate.mnemonic) {
            let op = match op {
                ModifierOp::Rep(_) => intermediate
                    .z
                    .map(ModifierOp::Rep)
                    .ok_or_else(|| anyhow!("need a Z flag specified for REP op")),
                _ => Ok(op),
            }?;
            self.modifier = Some(op);
            Ok(None)
        } else if let Ok(op) = ControlXferOp::from_str(&intermediate.mnemonic) {
            let code = intermediate.code.load::<u8>();
            let value = match code {
                0b11000010 | 0b11001101 | 0b11001010 => {
                    let imd = self.parse_literal(code != 0b11001101)?;
                    Some(imd)
                }
                _ => None,
            };
            Ok(Some(Instr::ControlXferInstr { op, value }))
        } else {
            Err(anyhow!("{}", intermediate.mnemonic))
        }
    }

    fn disassemble_unary_op(
        &mut self,
        op: UnaryOp,
        intermediate: &IntermediateInstr,
    ) -> anyhow::Result<Instr> {
        let (dst, wide) =
            if let Some((rm, mm, w)) = (intermediate.rm, intermediate.mm, intermediate.w).zip() {
                let rm = self.parse_rm(rm, mm, Some(w))?;
                Ok((UnaryDest::RM(rm), Some(w)))
            } else if let Some(reg) = intermediate.reg {
                let reg = parse_register(reg, true)?;
                Ok((UnaryDest::RM(RM::Reg(reg)), None))
            } else if let Some(sr) = intermediate.sr {
                let segreg = parse_segment(sr)?;
                Ok((UnaryDest::RM(RM::SegReg(segreg)), None))
            } else if let Some((start, end)) = intermediate.start.zip(intermediate.end) {
                Ok((UnaryDest::InterSeg((start, end)), None))
            } else if let Some(offset) = intermediate.offset {
                Ok((UnaryDest::Offset(offset), None))
            } else {
                Err(anyhow!("unhandled unary instr: {:?}", intermediate))
            }?;
        Ok(Instr::UnaryInstr { op, dst, wide })
    }

    fn disassemble_binary_op(
        &mut self,
        op: BinaryOp,
        intermediate: &IntermediateInstr,
    ) -> anyhow::Result<Instr> {
        let (dst, src) = if let Some((rm, mm)) = intermediate.rm.zip(intermediate.mm) {
            let rm = self.parse_rm(rm, mm, intermediate.w)?;
            if let Some((reg, w)) = intermediate.reg.zip(intermediate.w) {
                let reg = parse_register(reg, w)?;
                let d = intermediate
                    .d
                    // Special cases where `d` is implied.
                    .unwrap_or(!matches!(op, BinaryOp::Test));
                if d {
                    Ok((RM::Reg(reg), rm))
                } else {
                    Ok((rm, RM::Reg(reg)))
                }
            } else if let Some(sr) = intermediate.sr {
                let segreg = RM::SegReg(parse_segment(sr)?);
                if intermediate.code.load::<u8>() == 0b10001100 {
                    // Weird one-off for MOV
                    Ok((rm, segreg))
                } else {
                    Ok((segreg, rm))
                }
            } else if let Some(reg) = intermediate.reg {
                let reg = parse_register(reg, true)?;
                Ok((RM::Reg(reg), rm))
            } else if let (Some(w), true) = (
                intermediate.w,
                intermediate.s.is_some() || matches!(op, BinaryOp::Mov),
            ) {
                let s = intermediate.s.unwrap_or(false);
                let data = if w && !s {
                    self.parse_literal(true)?
                } else {
                    let mut literal = self.parse_literal(false)?;
                    if w {
                        literal = literal.as_word();
                    }
                    literal
                };
                Ok((rm, RM::Imd(data)))
            } else {
                Err(anyhow!("unhandled binary instr: {:?}", intermediate))
            }
        } else if let Some(w) = intermediate.w {
            if let Some(reg) = intermediate.reg {
                let reg = parse_register(reg, w)?;
                let imd = self.parse_literal(w)?;
                Ok((RM::Reg(reg), RM::Imd(imd)))
            } else {
                // When only W present, there are more "special case" treatments
                // that can only really be determined by looking at the prefix.
                let reg = if w { Register::AX } else { Register::AL };
                let code = intermediate.code.load::<u8>();
                match code {
                    0b1110010 | 0b1110011 => {
                        let data = self.parse_literal(false)?;
                        if matches!(op, BinaryOp::In) {
                            Ok((RM::Reg(reg), RM::Imd(data)))
                        } else {
                            Ok((RM::Imd(data), RM::Reg(reg)))
                        }
                    }
                    0b1110110 | 0b1110111 => {
                        if matches!(op, BinaryOp::In) {
                            Ok((RM::Reg(reg), RM::Reg(Register::DX)))
                        } else {
                            Ok((RM::Reg(Register::DX), RM::Reg(reg)))
                        }
                    }
                    0b1010000 | 0b1010001 => {
                        let data = self.parse_literal(w)?;
                        if code == 0b1010001 {
                            Ok((
                                RM::Mem(MemAddr::new(None, None, None, Some(data))),
                                RM::Reg(reg),
                            ))
                        } else {
                            Ok((
                                RM::Reg(reg),
                                RM::Mem(MemAddr::new(None, None, None, Some(data))),
                            ))
                        }
                    }
                    _ => {
                        let data = self.parse_literal(w)?;
                        Ok((RM::Reg(reg), RM::Imd(data)))
                    }
                }
            }
        } else if let (Some(reg), true) = (intermediate.reg, matches!(op, BinaryOp::Xchg)) {
            // Special case for XCHG
            let reg = parse_register(reg, true)?;
            Ok((RM::Reg(Register::AX), RM::Reg(reg)))
        } else {
            Err(anyhow!("unhandled unary instr: {:?}", intermediate))
        }?;
        Ok(Instr::BinaryInstr { op, dst, src })
    }
}

/// Parse the single next byte as a "literal."
fn parse_literal_byte(bits: &BitS) -> (&BitS, Literal) {
    let (head, tail) = bits.split_at(8);
    (tail, Literal::Byte(head.load()))
}

fn parse_literal_word(bits: &BitS) -> (&BitS, Literal) {
    let (head, tail) = bits.split_at(16);
    (tail, Literal::Word(head.load()))
}

fn parse_offset_byte(bits: &BitS) -> (&BitS, Offset) {
    let (head, tail) = bits.split_at(8);
    (tail, Offset::Byte(head.load::<i8>()))
}

fn parse_offset_word(bits: &BitS) -> (&BitS, Offset) {
    let (head, tail) = bits.split_at(16);
    (tail, Offset::Word(head.load::<i16>()))
}

fn parse_mod_mode(mm: u8) -> anyhow::Result<MovMode> {
    CODE_TO_MOD_MODE
        .get(&mm)
        .copied()
        .ok_or_else(|| anyhow!("invalid input code"))
}

/// Match a byte `code` to a `Register`.
fn parse_register(code: u8, wide: bool) -> anyhow::Result<Register> {
    if wide {
        CODE_TO_REGISTER_WIDE
            .get(&code)
            .copied()
            .ok_or_else(|| anyhow!("Unexpected register code {:?}", code))
    } else {
        CODE_TO_REGISTER
            .get(&code)
            .copied()
            .ok_or_else(|| anyhow!("Unexpected register code {:?}", code))
    }
}

fn parse_effective_address(
    mode: &MovMode,
    code: u8,
    disp: Option<Literal>,
    segment: Option<Segment>,
) -> anyhow::Result<MemAddr> {
    let (reg1, reg2) = match code {
        0b000 => Ok((Some(Register::BX), Some(Register::SI))),
        0b001 => Ok((Some(Register::BX), Some(Register::DI))),
        0b010 => Ok((Some(Register::BP), Some(Register::SI))),
        0b011 => Ok((Some(Register::BP), Some(Register::DI))),
        0b100 => Ok((Some(Register::SI), None)),
        0b101 => Ok((Some(Register::DI), None)),
        0b110 => match mode {
            MovMode::MemMode(None) => Ok((None, None)),
            _ => Ok((Some(Register::BP), None)),
        },
        0b111 => Ok((Some(Register::BX), None)),
        _ => Err(anyhow!("invalid input code")),
    }?;
    Ok(MemAddr::new(segment, reg1, reg2, disp))
}

fn parse_segment(code: u8) -> anyhow::Result<Segment> {
    CODE_TO_SEGMENT
        .get(&code)
        .copied()
        .ok_or_else(|| anyhow!("Unexpected segment code {:?}", code))
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::*;

    lazy_static! {
        static ref PUSH: Spec = Spec {
            mnemonic: "PUSH".to_string(),
            code: bitvec![u8, Msb0; 1, 1, 1, 1, 1, 1, 1],
            rest: vec![
                InstructionPart::W,
                InstructionPart::MOD,
                InstructionPart::Literal(bitvec![u8, Msb0; 1, 1, 0]),
                InstructionPart::RM,
            ],
        };
    }

    #[test]
    fn test_generate_combinations() {
        let fields = vec![
            BitSchemaPart::Fixed(bitvec![u8, Msb0; 1]),
            BitSchemaPart::Variable(2),
            BitSchemaPart::Fixed(bitvec![u8, Msb0; 1]),
        ];
        assert_eq!(
            HashSet::<BitV>::from_iter(generate_combinations(&fields).iter().cloned()),
            HashSet::<BitV>::from_iter(
                [
                    bitvec![u8, Msb0; 1, 0, 0, 1],
                    bitvec![u8, Msb0; 1, 0, 1, 1],
                    bitvec![u8, Msb0; 1, 1, 0, 1],
                    bitvec![u8, Msb0; 1, 1, 1, 1]
                ]
                .iter()
                .cloned()
            )
        );
    }

    #[test]
    fn test_try_spec() {
        let bits = bits![u8, Msb0; 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1];
        let mut disassembler = Disassembler::new(bits);
        let intermediate = disassembler.try_spec(&PUSH).unwrap().unwrap();
        assert_eq!(intermediate.mnemonic, "PUSH");
        assert!(intermediate.w.unwrap());
        assert_eq!(intermediate.rm.unwrap(), 0b111);
        assert_eq!(intermediate.mm.unwrap(), 0b11);
    }

    #[test]
    fn test_parse_mod_mode() {
        assert_eq!(parse_mod_mode(0b00).unwrap(), MovMode::MemMode(None));
        assert_eq!(
            parse_mod_mode(0b01).unwrap(),
            MovMode::MemMode(Some(Displacement::D8))
        );
        assert_eq!(
            parse_mod_mode(0b10).unwrap(),
            MovMode::MemMode(Some(Displacement::D16))
        );
        assert_eq!(parse_mod_mode(0b11).unwrap(), MovMode::RegMode);
    }
}
