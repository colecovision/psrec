use crate::{
    util::obtain_le,
    unif::UnifyVar
};
use super::obj::ObjectData;

#[derive(Debug)]
pub struct Expr(Vec<ExprToken>);

#[derive(Clone, Copy, Debug)]
/// An element of an expression that may be patched into an executable.
enum ExprToken {
	/// A literal (constant) word.
	Literal(u32),
	/// A symbol (the u16 index).
	Symbol(u16),
	/// The starting address of a section (u16) of the object file.
	SectionBase(u16),
	/// The starting address of section (u16) of the executable.
	SectionStart(u16),
	/// The address after the end of section (u16) of the executable.
	SectionEnd(u16),
    /// Add the two following tokens.
    Add,
    /// Subtract the following token from the one after it.
    Sub,
    /// Divide by the following token the one after it.
    Div,
    /// Bitwise-OR the two following tokens.
    BitOr
}

macro_rules! check_size {
	($var:expr, $size:literal, $tag:literal) => {
		($var.len() >= $size).then(|| $var.split_at($size))
		                     .ok_or(concat!("Truncated ", $tag, " expr"))?
	}
}

impl Expr {
	pub(super) fn parse(mut data: &[u8]) -> Result<(Self, &[u8]), String> {
		use ExprToken::*;

        let mut elems = 1;
        let mut out = Vec::new();

        while elems > 0 {
            let (&expr, edata) = data.split_first()
                                     .ok_or("Truncated expr")?;

            let (elem, edata) = match expr {
                0 => {
                    let (lit, data) = check_size!(edata, 4, "literal");
                    (Literal(obtain_le(lit)), data)
                },
                2 => {
                    let (idx, data) = check_size!(edata, 2, "symbol");
                    (Symbol(obtain_le(idx)), data)
                },
                4 => {
                    let (idx, data) = check_size!(edata, 2, "section");
                    (SectionBase(obtain_le(idx)), data)
                },
                12 => {
                    let (idx, data) = check_size!(edata, 2, "section start");
                    (SectionStart(obtain_le(idx)), data)
                },
                22 => {
                    let (idx, data) = check_size!(edata, 2, "section end");
                    (SectionEnd(obtain_le(idx)), data)
                },
                44 => {
                    elems += 2;
                    (Add, edata)
                },
                46 => {
                    elems += 2;
                    (Sub, edata)
                },
                50 => {
                    elems += 2;
                    (Div, edata)
                },
                54 => {
                    elems += 2;
                    (BitOr, edata)
                },
                _ => return Err(format!("Unknown expr: {} env: {:?}", expr, out))
            };

            out.push(elem);
            data = edata;
            elems -= 1;
        }

        Ok((Self(out), data))
    }

    pub fn as_var(&self, name: usize, file: &ObjectData) -> (UnifyVar, u32) {
        let mut stack = Vec::new();

        use UnifyVar::*;

        for &token in self.0.iter().rev() {
            match token {
                ExprToken::Literal(lit) => stack.push((None, lit)),
                ExprToken::Symbol(idx) => stack.push((Some(
                    Symbol(file.sym_name_from_idx(idx).unwrap())
                ), 0)),
                ExprToken::SectionBase(idx) => stack.push((Some(
                    SecBase(name, file.sec_by_idx(idx).unwrap().name)
                ), 0)),
                ExprToken::SectionStart(idx) => stack.push((Some(
                    SecStart(file.sec_by_idx(idx).unwrap().name)
                ), 0)),
                ExprToken::SectionEnd(idx) => stack.push((Some(
                    SecEnd(file.sec_by_idx(idx).unwrap().name)
                ), 0)),
                ExprToken::Add => {
                    let (sec, o2) = stack.pop().unwrap();
                    let (fst, o1) = stack.pop().unwrap();

                    stack.push((match (fst, sec) {
                        (None, x) | (x, None) => x,
                        (Some(x1), Some(x2)) => unimplemented!("add {:?} and {:?}", x1, x2)
                    }, o1.wrapping_add(o2)));
                },
                ExprToken::Sub => {
                    let (sec, o2) = stack.pop().unwrap();
                    let (fst, o1) = stack.pop().unwrap();

                    stack.push((match (fst, sec) {
                        (x, None) => x,
                        (x, y) if x == y => None,
                        (Some(SecEnd(x)), Some(SecStart(y))) if x == y => Some(SecSizeBytes(x)),
                        (x, Some(y)) => unimplemented!("sub {:?} and {:?}", x, y)
                    }, o1.wrapping_sub(o2)));
                },
                _ => unimplemented!("what expr: {:?}", self)
            }
        }

        let q = if let (Some(var), off) = stack.drain(..).next().unwrap() {
            (var, off)
        } else {
            unimplemented!("pure offset!");
        };

        q
    }
}
