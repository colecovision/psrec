use std::{
    collections::HashMap,
    hint
};

use crate::{
    util::obtain_le,
    unif::UnifyState
};
use super::expr::Expr;

pub struct ObjectData {
	// Object file version must be 2.
	// version: u8,
	/// Processor type, if any, for this object file.
	/// If .text content is available, must be 7.
	pub proc: Option<Processor>,
	/// Map from section indices to sections present in the object file.
	pub secs: HashMap<u16, Section>,
	/// Map from symbol indices to external references to symbols.
	pub refs: HashMap<u16, String>,
	/// Map from file indices to files that contributed to this object.
	pub files: HashMap<u16, String>,
}

impl ObjectData {
	pub fn sec_by_idx(&self, idx: u16) -> Option<&Section> {
        self.secs.get(&idx)
	}

	pub fn sec_by_name(&self, name: &str) -> Option<&Section> {
		self.secs.values().filter(|x| x.name == name).next()
	}

	fn has_sym_by_idx(&self, idx: u16) -> bool {
		   self.refs.contains_key(&idx)
        || self.secs.values().any(|x| x.defs.contains_key(&idx))
	    || self.secs.values().any(|x| x.bss.contains_key(&idx))
	}

	fn has_sym_by_name(&self, name: &str) -> bool {
		   self.refs.values().any(|x| x == name)
        || self.secs.values().any(|x| x.defs.values().any(|x| x.name == name))
	    || self.secs.values().any(|x| x.bss.values().any(|x| x.name == name))
	}

	pub fn sym_name_from_idx(&self, idx: u16) -> Option<&str> {
	    self.refs.get(&idx).or_else(||
	        self.secs.values().find_map(|x| x.defs.get(&idx).map(|x| &x.name)
	                             .or_else(|| x.bss.get(&idx).map(|x| &x.name)))
	    ).map(|x| x.as_str())
	}
}

pub enum Processor {
	M68K, // 0
	WDC65816, // 1
	MOS6502, // 2
	Z80, // 3
	SPC700, // 4
	X86, // 5
	ARM, // 6
	R3K, // 7
	SH, // 8
	R4K5K // 9
}

impl TryFrom<u8> for Processor {
	type Error = String;

	fn try_from(x: u8) -> Result<Self, Self::Error> {
		Ok(match x {
			0 => Processor::M68K,
			1 => Processor::WDC65816,
			2 => Processor::MOS6502,
			3 => Processor::Z80,
			4 => Processor::SPC700,
			5 => Processor::X86,
			6 => Processor::ARM,
			7 => Processor::R3K,
			8 => Processor::SH,
			9 => Processor::R4K5K,
			_ => return Err(format!("Unknown processor: 0x{:02X}", x))
		})
	}
}

pub struct Section {
	/// Section group.
	_grp: u16,
	/// Section alignment, in bytes.
	pub align: u8,
	/// Section name.
	pub name: String,
	/// List of blocks in section.
	pub blocks: Vec<Block>,
	/// List of patches for section.
	pub patches: Vec<Patch>,
	/// Map from symbol indices to external defines defined in this section.
	pub defs: HashMap<u16, ExtDef>,
	/// Map from symbols indices to external BSS areas defined in this section.
	pub bss: HashMap<u16, ExtBss>,
	/// List of local defines defined in this section.
	locs: Vec<LocDef>
}

pub struct ExtDef {
	/// Offset into the section of symbol.
	pub off: u32,
	/// Symbol name.
	pub name: String
}

pub enum Block {
	/// Data blob.
	Code(Box<[u8]>),
	/// Size of uninitialised block, in bytes.
	Uninit(u32)
}

impl Block {
	pub fn len(&self) -> usize {
	    match self {
	        Block::Code(b) => b.len(),
	        Block::Uninit(x) => *x as usize
	    }
	}
}

pub struct ExtBss {
	/// Size of BSS area, in bytes.
	pub size: u32,
	/// Symbol name.\
	pub name: String
}

struct LocDef {
	/// Offset into the section of symbol.
	_off: u32,
	/// Symbol name.
	_name: String
}

pub struct Patch {
	/// Patch kind.
	pub kind: PatchKind,
	/// Offset into the section of patch.
	pub off: u16,
	/// Expression from which patch originates.
	pub expr: Expr
}

#[derive(Clone, Copy, Debug)]
pub enum PatchKind {
	Low28,
	Full,
	Upper16,
	Lower16,
}

impl TryFrom<u8> for PatchKind {
	type Error = String;

	fn try_from(x: u8) -> Result<Self, Self::Error> {
		Ok(match x {
			0x10 => PatchKind::Full,
			0x4A => PatchKind::Low28,
			0x52 => PatchKind::Upper16,
			0x54 => PatchKind::Lower16,
			_ => return Err(format!("Unknown patch kind: {:02X}", x))
        })
	}
}

impl PatchKind {
    pub fn as_state(self, patched: u32, offset: u32) -> UnifyState {
        use UnifyState::*;

        match self {
            PatchKind::Low28 => InRange(
                patched << 2 & 0xFFFFFFC | offset & 0xF0000000,
                0
            ),
            PatchKind::Full => InRange(patched, 0),
            PatchKind::Upper16 => InRange(
                (patched << 16).wrapping_sub(32768),
                65535
            ),
            PatchKind::Lower16 => Lower16(patched as u16)
        }
    }
}

macro_rules! check_size {
	($var:expr, $size:literal, $tag:literal) => {
		($var.len() >= $size).then(|| $var.split_at($size))
		                     .ok_or(concat!("Truncated ", $tag, " tag"))?
	}
}

impl ObjectData {
    pub fn parse(data: &[u8]) -> Result<ObjectData, String> {
        if data.len() < 4 {
            return Err("File area too short to be an object".to_string());
        }

        let (header, mut rest) = data.split_at(4);

        if header != b"LNK\x02" {
            return Err("Invalid header or object version".to_string());
        }

        let mut out = ObjectData {
            proc: None,
            secs: HashMap::new(),
            refs: HashMap::new(),
            files: HashMap::new()
        };

        let mut curr = None;

        loop {
			let (&tag, _rest) = rest.split_first()
			                        .ok_or("File area was truncated")?;

			rest = match tag {
				0 => {	
					if !_rest.is_empty() {
						return Err(format!("Spurious data in file area: 0x{:02X}, 0x{:02X}", _rest[0], _rest[1]));
					}

					break;
				},
				2 => {
					let (len, _rest) = check_size!(_rest, 2, "code block");
					let len = obtain_le::<u16, 2, _, _>(len) as usize;

					if len == 0 {
						return Err("Invalid length for code block tag".to_string());
					}

					if _rest.len() < len {
						return Err("Truncated code block data".to_string());
					}

					let (data, _rest) = _rest.split_at(len);

					let idx = curr.ok_or("Code block with no selected section")?;

					out.secs.get_mut(&idx)
					   .unwrap_or_else(|| unsafe { hint::unreachable_unchecked() })
					   .blocks
					   .push(Block::Code(Box::from(data)));

					_rest
				},
				6 => {
					let (idx, _rest) = check_size!(_rest, 2, "section select");
					let idx = obtain_le(idx);

					if !out.secs.contains_key(&idx) {
						return Err("Undefined section selected".to_string());
					}

					curr = Some(idx);
					_rest
				},
				8 => {
					let (size, _rest) = check_size!(_rest, 4, "uninit block");
					let size = obtain_le(size);

					if size == 0 {
						return Err("Invalid length for uninit block tag".to_string());
					}

					let idx = curr.ok_or("Uninit block with no selected section")?;
					
					out.secs.get_mut(&idx)
					   .unwrap_or_else(|| unsafe { hint::unreachable_unchecked() })
					   .blocks
					   .push(Block::Uninit(size));

					_rest
				},
				10 => {
					let (head, _rest) = check_size!(_rest, 3, "patch");
					let kind = PatchKind::try_from(head[0])?;
					let off = obtain_le(&head[1..]);
					let (expr, _rest) = Expr::parse(_rest)?;

					let idx = curr.ok_or("Patch with no selected section")?;

					out.secs.get_mut(&idx)
					   .unwrap_or_else(|| unsafe { hint::unreachable_unchecked() })
					   .patches
					   .push(Patch {
						kind,
						off,
						expr
					});

					_rest
				},
				12 => {
					let (head, _rest) = check_size!(_rest, 9, "extdef");
					let idx = obtain_le(head);

					if out.has_sym_by_idx(idx) {
						return Err("Two symbols with same index".to_string());
					}

					let off = obtain_le(&head[4..]);
					let len = head[8] as usize;

					if _rest.len() < len {
						return Err("Truncated extdef name".to_string());
					}

					let (name, _rest) = _rest.split_at(len);
					let name = String::from_utf8(Vec::from(name)).map_err(|_| "Invalid extdef name data".to_string())?;

					if out.has_sym_by_name(&name) {
						return Err("Two symbols with same name".to_string());
					}

					let sec = out.secs.get_mut(&obtain_le(&head[2..]))
					             .ok_or("Extdef for yet undefined section")?;

					sec.defs.insert(idx, ExtDef {
						off,
						name
					});

					_rest
				},
				14 => {
					let (head, _rest) = check_size!(_rest, 3, "extref");
					let idx = obtain_le(head);

					if out.has_sym_by_idx(idx) {
						return Err("Two symbols with same index".to_string());
					}

					let len = head[2] as usize;

					if _rest.len() < len {
						return Err("Truncated extdef name".to_string());
					}

					let (name, _rest) = _rest.split_at(len);
					let name = String::from_utf8(Vec::from(name)).map_err(|_| "Invalid extref name data".to_string())?;

					if out.has_sym_by_name(&name) {
						return Err("Two symbols with same name".to_string());
					}

					out.refs.insert(idx, name);

					_rest
				},
				16 => {
					let (head, _rest) = check_size!(_rest, 6, "section");
					let idx = obtain_le(head);

                    if out.secs.contains_key(&idx) {
                        return Err("Two sections with same index".to_string());
                    }

					let grp = obtain_le(&head[2..]);
					let align = head[4];
					let len = head[5] as usize;

					if _rest.len() < len {
						return Err("Truncated section name".to_string());
					}

					let (name, _rest) = _rest.split_at(len);
					let name = String::from_utf8(Vec::from(name)).map_err(|_| "Invalid section name data".to_string())?;

                    if out.secs.values().any(|x| x.name == name) {
                        return Err("Two sections with same name".to_string());
                    }

					out.secs.insert(idx, Section {
						_grp: grp,
						align,
						name,
						blocks: Vec::new(),
						patches: Vec::new(),
						defs: HashMap::new(),
						bss: HashMap::new(),
						locs: Vec::new()
					});

					_rest
				},
				18 => {
					let (head, _rest) = check_size!(_rest, 7, "locdef");

					let off = obtain_le(&head[2..]);
					let len = head[6] as usize;

					if _rest.len() < len {
						return Err("Truncated locdef name".to_string());
					}

					let (name, _rest) = _rest.split_at(len);
					let name = String::from_utf8(Vec::from(name)).map_err(|_| "Invalid locdef name data".to_string())?;

					let sec = out.secs.get_mut(&obtain_le(head))
					             .ok_or("Locdef for yet undefined section")?;

					sec.locs.push(LocDef {
						_off: off,
						_name: name
					});

					_rest
				},
				28 => {
					let (head, _rest) = check_size!(_rest, 3, "file");
					let idx = obtain_le(head);

					if out.files.contains_key(&idx) {
						return Err("Two files with same index".to_string());
					}

					let len = head[2] as usize;

					if _rest.len() < len {
						return Err("Truncated file name".to_string());
					}

					let (name, _rest) = _rest.split_at(len);
					let name = String::from_utf8(Vec::from(name)).map_err(|_| "Invalid file name data".to_string())?;

					out.files.insert(idx, name);

					_rest
				},
				46 => {
					let (proc, _rest) = check_size!(_rest, 1, "processor type");

					if out.proc.is_some() {
						return Err("Multiple processors in object".to_string());
					}

					out.proc = Some(Processor::try_from(proc[0])?);
					_rest
				},
				48 => {
					let (head, _rest) = check_size!(_rest, 9, "extbss");
					let idx = obtain_le(head);

					if out.has_sym_by_idx(idx) {
						return Err("Two symbols with same index".to_string());
					}

					let size = obtain_le(&head[4..]);
					let len = head[8] as usize;

					if _rest.len() < len {
						return Err("Truncated extbss name".to_string());
					}

					let (name, _rest) = _rest.split_at(len);
					let name = String::from_utf8(Vec::from(name)).map_err(|_| "Invalid extbss name data")?;

					if out.has_sym_by_name(&name) {
						return Err("Two symbols with same name".to_string());
					}

					let sec = out.secs.get_mut(&obtain_le(&head[2..]))
								 .ok_or("Extbss for yet undefined section")?;

					sec.bss.insert(idx, ExtBss {
						size,
						name
					});

					_rest
				},
				_ => {
					return Err(format!("Unknown tag: {}", tag));
				}
			}
        }

        Ok(out)
    }
}
