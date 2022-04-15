use crate::util::obtain_le;

#[derive(Debug)]
/// An expression that may be patched into an executable.
pub enum Expr {
	/// A literal (constant) word.
	Literal(u32),
	/// A symbol (the u16 index) plus some constant.
	SymbolPlus(u16, u32),
	/// A constant *minus* a symbol.
	SymbolMinus(u16, u32),
	/// Some constant offset into a local section (the u16 index).
	SectionPlus(u16, u32),
	/// The start of a global section.
	SectionStart(u16),
	/// The end of a global section.
	SectionEnd(u16),
	/// The size of a global section, in words.
	SectionWords(u16),
	/// The size of a global section, in bytes.
	SectionBytes(u16),
	/// Some constant offset into a local section, coerced to KSEG1 access.
	KSeg1Offset(u16, u32)
}

macro_rules! check_size {
	($var:expr, $size:literal, $tag:literal) => {
		($var.len() < $size).then(|| $var.split_at($size))
		                    .ok_or(concat!("Truncated ", $tag, " expr"))?
	}
}

impl Expr {
	pub(super) fn parse(data: &[u8]) -> Result<(Self, &[u8]), String> {
		use Expr::*;

		let (&expr, data) = data.split_first()
			                    .ok_or("Truncated expr")?;

		Ok(match expr {
			0 => {
				let (lit, data) = check_size!(data, 4, "literal");
				(Literal(obtain_le(lit)), data)
			},
			2 => {
				let (idx, data) = check_size!(data, 2, "symbol");
				(SymbolPlus(obtain_le(idx), 0), data)
			},
			4 => {
				let (idx, data) = check_size!(data, 2, "section");
				(SectionPlus(obtain_le(idx), 0), data)
			},
			12 => {
				let (idx, data) = check_size!(data, 2, "section start");
				(SectionStart(obtain_le(idx)), data)
			},
			22 => {
				let (idx, data) = check_size!(data, 2, "section end");
				(SectionEnd(obtain_le(idx)), data)
			},
			44 => {
				let (edata, data) = check_size!(data, 8, "+");

				let expr = if edata[0] == 0 && edata[5] == 4 {
					SectionPlus(obtain_le(&edata[6..]), obtain_le(&edata[1..]))
				} else if edata[0] == 4 && edata[3] == 0 {
					SectionPlus(obtain_le(&edata[1..]), obtain_le(&edata[4..]))
				} else if edata[0] == 0 && edata[5] == 2 {
					SymbolPlus(obtain_le(&edata[6..]), obtain_le(&edata[1..]))
				} else if edata[0] == 2 && edata[3] == 0 {
					SymbolPlus(obtain_le(&edata[1..]), obtain_le(&edata[4..]))
				} else {
					return Err("Unknown + expr in patch".to_string());
				};

				(expr, data)
			},
			46 => {
				let (expr, data) = if data.len() >= 8 && data[0] == 2 && data[3] == 0 {
					let (edata, data) = data.split_at(8);
					(SymbolMinus(obtain_le(&edata[1..]), obtain_le(&edata[4..])), data)
				} else if data.len() >= 8 && data[0] == 0 && data[5] == 2 {
					let (edata, data) = data.split_at(8);
					(SymbolPlus(obtain_le(&edata[6..]), -obtain_le::<i32, 4, _, _>(&edata[1..]) as u32), data)
				} else if data.len() >= 6 && data[0] == 12 && data[3] == 22 && data[1..3] == data[4..6] {
					let (edata, data) = data.split_at(6);
					(SectionBytes(obtain_le(&edata[1..])), data)
				} else {
					return Err("Unknown - expr in patch".to_string());
				};

				(expr, data)
			},
			50 => {
				let (edata, data) = check_size!(data, 12, "/");

				let expr = if &edata[0..7] == b"\x00\x04\x00\x00\x00\x2E\x0C" && edata[9] == 22 && edata[7..9] == edata[10..12] {
					SectionWords(obtain_le(&data[10..]))
				} else {
					return Err("Unknown / expr in patch".to_string());
				};

				(expr, data)
			},
			54 => {
				let (edata, data) = check_size!(data, 14, "!");

				let expr = if &edata[0..7] == b"\x00\x00\x00\x00\xA0\x2C\x04" && edata[9] == 0 {
					KSeg1Offset(obtain_le(&edata[7..]), obtain_le(&edata[10..]))
				} else {
					return Err("Unknown ! expr in patch".to_string());
				};

				(expr, data)
			},
			_ => return Err(format!("Unknown expr in patch: {}", expr))
		})
    }
}
