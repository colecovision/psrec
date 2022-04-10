use std::{
	ops::Range,
	hint
};

use crate::util::obtain_le;

pub struct Executable {
	pc0: u32,
	gp0: u32,
	pub text: (u32, Box<[u8]>),
	// data is always zero
	bss: Range<u32>,
	stack: Range<u32>
}

pub fn parse_exe(data: &[u8]) -> Result<Executable, String> {
	if data.len() & 2047 != 0 || data.len() < 2048 {
		return Err("Invalid alignment for executable".to_string());
	}

	let (head, data) = data.split_at(2048);

	if &head[..16] != b"PS-X EXE\x00\x00\x00\x00\x00\x00\x00\x00" {
		return Err("Invalid header for executable".to_string());
	}

	if &head[32..40] != b"\x00\x00\x00\x00\x00\x00\x00\x00" {
		return Err("d_* in executable header nonzero??? WHAT???".to_string());
	}

	let b_addr = obtain_le(&head[40..]);
	let b_size: u32 = obtain_le(&head[44..]);

	let s_addr = obtain_le(&head[48..]);
	let s_size: u32 = obtain_le(&head[52..]);

	Ok(Executable {
		pc0: obtain_le(&head[16..]),
		gp0: obtain_le(&head[20..]),
		text: (obtain_le(&head[24..]), Box::from(&data[..obtain_le::<u32, _, _, 4>(&head[28..]) as usize])),
		bss: b_addr .. b_addr + b_size,
		stack: s_addr .. s_addr + s_size
	})
}

pub struct ObjectData {
	// Object file version must be 2.
	// version: u8,
	/// Processor type, if any, for this object file.
	/// If .text content is available, must be 7.
	pub proc: Option<Processor>,
	/// List of sections present in the object file.
	pub secs: Vec<Section>,
	/// List of external references to symbols.
	pub refs: Vec<ExtRef>,
	/// List of files that contributed to this object.
	pub files: Vec<FileName>,
}

impl ObjectData {
	pub fn sec_by_idx(&self, idx: u16) -> Option<&Section> {
		self.secs.iter().filter(|x| x.idx == idx).next()
	}

	fn sec_by_idx_mut(&mut self, idx: u16) -> Option<&mut Section> {
		self.secs.iter_mut().filter(|x| x.idx == idx).next()
	}

	pub fn sec_by_name(&self, name: &str) -> Option<&Section> {
		self.secs.iter().filter(|x| x.name == name).next()
	}

	fn sec_by_name_mut(&mut self, name: &str) -> Option<&mut Section> {
		self.secs.iter_mut().filter(|x| x.name == name).next()
	}

	fn has_sym_by_idx(&self, idx: u16) -> bool {
		self.refs.iter().any(|x| x.idx == idx) || self.secs.iter().any(|x| x.def_by_idx(idx).is_some())
	                                           || self.secs.iter().any(|x| x.bss_by_idx(idx).is_some())
	}

	fn has_sym_by_name(&self, name: &str) -> bool {
		self.refs.iter().any(|x| x.name == name) || self.secs.iter().any(|x| x.def_by_name(name).is_some())
	                                             || self.secs.iter().any(|x| x.bss_by_name(name).is_some())
	}

	pub fn sym_name_from_idx(&self, idx: u16) -> Option<&str> {
	    self.refs.iter().find_map(|x| (x.idx == idx).then(|| &x.name)).or_else(||
	        self.secs.iter().find_map(|x| x.def_by_idx(idx).map(|x| &x.name)
	                                       .or_else(|| x.bss_by_idx(idx).map(|x| &x.name)))
	    ).map(|x| x.as_str())
	}

	fn has_file(&self, idx: u16) -> bool {
	    self.files.iter().any(|x| x.idx == idx)
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

pub struct FileName {
	/// File index.
	idx: u16,
	/// File name.
	pub name: String
}

pub struct ExtRef {
	/// Symbol index.
	idx: u16,
	/// Symbol name.
	pub name: String
}

pub struct Section {
	/// Section index.
	idx: u16,
	/// Section group.
	grp: u16,
	/// Section alignment, in bytes.
	pub align: u8,
	/// Section name.
	pub name: String,
	/// List of blocks in section.
	pub blocks: Vec<Block>,
	/// List of patches for section.
	pub patches: Vec<Patch>,
	/// List of external defines defined in this section.
	pub defs: Vec<ExtDef>,
	/// List of external BSS areas defined in this section.
	pub bss: Vec<ExtBss>,
	/// List of local defines defined in this section.
	locs: Vec<LocDef>
}

impl Section {
	fn def_by_idx(&self, idx: u16) -> Option<&ExtDef> {
		self.defs.iter().find(|x| x.idx == idx)
	}

	fn def_by_name(&self, name: &str) -> Option<&ExtDef> {
		self.defs.iter().find(|x| x.name == name)
	}

	fn bss_by_idx(&self, idx: u16) -> Option<&ExtBss> {
		self.bss.iter().find(|x| x.idx == idx)
	}

	fn bss_by_name(&self, name: &str) -> Option<&ExtBss> {
		self.bss.iter().find(|x| x.name == name)
	}
}

pub struct ExtDef {
	/// Symbol index.
	idx: u16,
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
	/// Symbol index.
	idx: u16,
	/// Size of BSS area, in bytes.
	pub size: u32,
	/// Symbol name.\
	pub name: String
}

struct LocDef {
	/// Offset into the section of symbol.
	off: u32,
	/// Symbol name.
	name: String
}

pub struct Patch {
	/// Patch kind.
	pub kind: PatchKind,
	/// Offset into the section of patch.
	pub off: u16,
	/// Expression from which patch originates.
	pub expr: Expr
}

#[derive(Debug)]
pub enum PatchKind {
	Low28,
	Full,
	Upper16,
	Lower16,
}

#[derive(Debug)]
pub enum Expr {
	Literal(u32),
	SymbolPlus(u16, u32),
	SymbolMinus(u16, u32),
	SectionPlus(u16, u32),
	SectionStart(u16),
	SectionEnd(u16),
	SectionWords(u16),
	SectionBytes(u16),
	KSeg1Offset(u16, u32)
}

pub fn parse_object(data: &[u8]) -> Result<ObjectData, String> {
	if data.len() < 4 {
		return Err("File area too short to be an object".to_string());
	}

	let (header, mut rest) = data.split_at(4);

	if header != b"LNK\x02" {
		return Err("Invalid header or object version".to_string());
	}

	let mut out = ObjectData {
		proc: None,
		secs: Vec::new(),
		refs: Vec::new(),
	    files: Vec::new()
	};

	let mut curr = None;

	loop {
		if let Some((&tag, _rest)) = rest.split_first() {
			rest = match tag {
				0 => {	
					if !_rest.is_empty() {
						return Err(format!("Spurious data in file area: 0x{:02X}, 0x{:02X}", _rest[0], _rest[1]));
					}

					break;
				},
	            2 => {
	                if _rest.len() < 2 {
	                    return Err("Truncated code block tag".to_string());
	                }

	                let (len, _rest) = _rest.split_at(2);
	                let len = obtain_le::<u16, _, _, 2>(len) as usize;

	                if len == 0 {
	                    return Err("Invalid length for code block tag".to_string());
	                }

	                if _rest.len() < len {
	                    return Err("Truncated code block data".to_string());
	                }

	                let (data, _rest) = _rest.split_at(len);

	                if let Some(idx) = curr {
	                    out.sec_by_idx_mut(idx).unwrap_or_else(|| unsafe { hint::unreachable_unchecked() }).blocks.push(Block::Code(Box::from(data)));
	                } else {
	                    return Err("Code block with no selected section".to_string());
	                }

	                _rest
	            },
				6 => {
					if _rest.len() < 2 {
						return Err("Truncated section select tag".to_string());
					}

					let (idx, _rest) = _rest.split_at(2);
					let idx = obtain_le(idx);

					if out.sec_by_idx(idx).is_none() {
						return Err("Undefined section selected".to_string());
					}

					curr = Some(idx);
					_rest
				},
	            8 => {
	                if _rest.len() < 4 {
	                    return Err("Truncated uninit block tag".to_string());
	                }

	                let (size, _rest) = _rest.split_at(4);
	                let size = obtain_le(size);

	                if size == 0 {
	                    return Err("Invalid length for uninit block tag".to_string());
	                }

	                if let Some(idx) = curr {
	                    out.sec_by_idx_mut(idx).unwrap_or_else(|| unsafe { hint::unreachable_unchecked() }).blocks.push(Block::Uninit(size));
	                } else {
	                    return Err("Uninit block with no selected section".to_string());
	                }

	                _rest
	            },
	            10 => {
	                if _rest.len() < 3 {
	                    return Err("Truncated patch tag".to_string());
	                }

	                let (head, _rest) = _rest.split_at(3);
	                let kind = match head[0] {
	                    0x10 => PatchKind::Full,
	                    0x4A => PatchKind::Low28,
	                    0x52 => PatchKind::Upper16,
	                    0x54 => PatchKind::Lower16,
	                    _ => return Err(format!("Unknown patch kind: {:02X}", head[0]))
	                };
	                let off = obtain_le(&head[1..]);

	                if let Some((&expr, _rest)) = _rest.split_first() {
	                    let (expr, _rest) = match expr {
	                        0 => {
	                            if _rest.len() < 4 {
	                                return Err("Truncated literal expr in patch".to_string());
	                            }

	                            let (lit, _rest) = _rest.split_at(4);
	                            (Expr::Literal(obtain_le(lit)), _rest)
	                        },
	                        2 => {
	                            if _rest.len() < 2 {
	                                return Err("Truncated symbol expr in patch".to_string());
	                            }

	                            let (idx, _rest) = _rest.split_at(2);
	                            (Expr::SymbolPlus(obtain_le(idx), 0), _rest)
	                        },
	                        4 => {
	                            if _rest.len() < 2 {
	                                return Err("Truncated section expr in patch".to_string());
	                            }

	                            let (idx, _rest) = _rest.split_at(2);
	                            (Expr::SectionPlus(obtain_le(idx), 0), _rest)
	                        },
	                        12 => {
	                            if _rest.len() < 2 {
	                                return Err("Truncated section start expr in patch".to_string());
	                            }

	                            let (idx, _rest) = _rest.split_at(2);
	                            (Expr::SectionStart(obtain_le(idx)), _rest)
	                        },
	                        22 => {
	                            if _rest.len() < 2 {
	                                return Err("Truncated section end expr in patch".to_string());
	                            }

	                            let (idx, _rest) = _rest.split_at(2);
	                            (Expr::SectionEnd(obtain_le(idx)), _rest)
	                        },
	                        44 => {
	                            if _rest.len() < 8 {
	                                return Err("Truncated + expr in patch".to_string());
	                            }

	                            let (data, _rest) = _rest.split_at(8);

	                            let expr = if data[0] == 0 && data[5] == 4 {
	                                Expr::SectionPlus(obtain_le(&data[6..]), obtain_le(&data[1..]))
	                            } else if data[0] == 4 && data[3] == 0 {
	                                Expr::SectionPlus(obtain_le(&data[1..]), obtain_le(&data[4..]))
	                            } else if data[0] == 0 && data[5] == 2 {
	                                Expr::SymbolPlus(obtain_le(&data[6..]), obtain_le(&data[1..]))
	                            } else if data[0] == 2 && data[3] == 0 {
	                                Expr::SymbolPlus(obtain_le(&data[1..]), obtain_le(&data[4..]))
	                            } else {
	                                return Err("Unknown + expr in patch".to_string());
	                            };

	                            (expr, _rest)
	                        },
	                        46 => {
	                            let (expr, _rest) = if _rest.len() >= 8 && _rest[0] == 2 && _rest[3] == 0 {
	                                let (data, _rest) = _rest.split_at(8);
	                                (Expr::SymbolMinus(obtain_le(&data[1..]), obtain_le(&data[4..])), _rest)
	                            } else if _rest.len() >= 8 && _rest[0] == 0 && _rest[5] == 2 {
	                                let (data, _rest) = _rest.split_at(8);
	                                (Expr::SymbolPlus(obtain_le(&data[6..]), -obtain_le::<i32, _, _, 4>(&data[1..]) as u32), _rest)
	                            } else if _rest.len() >= 6 && _rest[0] == 12 && _rest[3] == 22 && _rest[1..3] == _rest[4..6] {
	                                let (data, _rest) = _rest.split_at(6);
	                                (Expr::SectionBytes(obtain_le(&data[1..])), _rest)
	                            } else {
	                                return Err("Unknown - expr in patch".to_string());
	                            };

	                            (expr, _rest)
	                        },
	                        50 => {
	                            if _rest.len() < 12 {
	                                return Err("Truncated / expr in patch".to_string());
	                            }

	                            let (data, _rest) = _rest.split_at(12);

	                            let expr = if &data[0..7] == b"\x00\x04\x00\x00\x00\x2E\x0C" && data[9] == 22 && data[7..9] == data[10..12] {
	                                Expr::SectionWords(obtain_le(&data[10..]))
	                            } else {
	                                return Err("Unknown / expr in patch".to_string());
	                            };

	                            (expr, _rest)
	                        },
	                        54 => {
	                            if _rest.len() < 14 {
	                                return Err("Truncated ! expr in patch".to_string());
	                            }

	                            let (data, _rest) = _rest.split_at(14);

	                            let expr = if &data[0..7] == b"\x00\x00\x00\x00\xA0\x2C\x04" && data[9] == 0 {
	                                Expr::KSeg1Offset(obtain_le(&data[7..]), obtain_le(&data[10..]))
	                            } else {
	                                return Err("Unknown ! expr in patch".to_string());
	                            };

	                            (expr, _rest)
	                        },
	                        _ => return Err(format!("Unknown expr in patch: {}", expr))
	                    };

	                    if let Some(idx) = curr {
	                        out.sec_by_idx_mut(idx).unwrap_or_else(|| unsafe { hint::unreachable_unchecked() }).patches.push(Patch {
	                            kind,
	                            off,
	                            expr
	                        });
	                    } else {
	                        return Err("Patch with no selected section".to_string());
	                    }

	                    _rest
	                } else {
	                    return Err("Truncated expr in patch tag".to_string());
	                }
	            },
				12 => {
					if _rest.len() < 9 {
						return Err("Truncated extdef tag".to_string());
					}

					let (head, _rest) = _rest.split_at(9);
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

					let sec = if let Some(sec) = out.sec_by_idx_mut(obtain_le(&head[2..])) {
						sec
					} else {
						return Err("Extdef for yet undefined section".to_string());
					};

					sec.defs.push(ExtDef {
						idx,
						off,
						name
					});

					_rest
				},
				14 => {
					if _rest.len() < 3 {
						return Err("Truncated extref tag".to_string());
					}

					let (head, _rest) = _rest.split_at(3);
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

					out.refs.push(ExtRef {
						idx,
						name
					});

					_rest
				},
				16 => {
					if _rest.len() < 6 {
						return Err("Truncated section tag".to_string());
					}

					let (head, _rest) = _rest.split_at(6);
					let idx = obtain_le(head);

					if out.sec_by_idx(idx).is_some() {
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

					if out.sec_by_name(&name).is_some() {
						return Err("Two sections with same name".to_string());
					}

					out.secs.push(Section {
						idx,
						grp,
						align,
						name,
						blocks: Vec::new(),
						patches: Vec::new(),
						defs: Vec::new(),
	                    bss: Vec::new(),
	                    locs: Vec::new()
					});

					_rest
				},
				18 => {
					if _rest.len() < 7 {
						return Err("Truncated locdef tag".to_string());
					}

					let (head, _rest) = _rest.split_at(7);

					let off = obtain_le(&head[2..]);
					let len = head[6] as usize;

					if _rest.len() < len {
						return Err("Truncated locdef name".to_string());
					}

					let (name, _rest) = _rest.split_at(len);
					let name = String::from_utf8(Vec::from(name)).map_err(|_| "Invalid locdef name data".to_string())?;

					let sec = if let Some(sec) = out.sec_by_idx_mut(obtain_le(head)) {
						sec
					} else {
						return Err("Locdef for yet undefined section".to_string());
					};

					sec.locs.push(LocDef {
						off,
						name
					});

					_rest
				},
				28 => {
					if _rest.len() < 3 {
						return Err("Truncated file tag".to_string());
					}

					let (head, _rest) = _rest.split_at(3);
					let idx = obtain_le(head);

					if out.has_file(idx) {
						return Err("Two files with same index".to_string());
					}

					let len = head[2] as usize;

					if _rest.len() < len {
						return Err("Truncated file name".to_string());
					}

					let (name, _rest) = _rest.split_at(len);
					let name = String::from_utf8(Vec::from(name)).map_err(|_| "Invalid file name data".to_string())?;

					out.files.push(FileName {
						idx,
						name
					});

					_rest
				},
				46 => {
					if _rest.is_empty() {
						return Err("Truncated processor type tag".to_string());
					}

					let (proc, _rest) = _rest.split_at(1);

					if out.proc.is_some() {
						return Err("Multiple processors in object".to_string());
					}

					out.proc = Some(match proc[0] {
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
						_ => return Err(format!("Unknown processor: 0x{:02X}", proc[0]))
					});
					_rest
				},
				48 => {
					if _rest.len() < 9 {
						return Err("Truncated extbss tag".to_string());
					}

					let (head, _rest) = _rest.split_at(9);
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
					let name = String::from_utf8(Vec::from(name)).map_err(|_| "Invalid extbss name data".to_string())?;

					if out.has_sym_by_name(&name) {
						return Err("Two symbols with same name".to_string());
					}

					let sec = if let Some(sec) = out.sec_by_idx_mut(obtain_le(&head[2..])) {
						sec
					} else {
						return Err("Extbss for yet undefined section".to_string());
					};

					sec.bss.push(ExtBss {
						idx,
						size,
						name
					});

					_rest
				},
				_ => {
					return Err(format!("Unknown tag: {}", tag));
				}
			}
		} else {
			return Err("File area was truncated".to_string());
		}
	}

	Ok(out)
}

pub struct ObjectFile {
	/// File name; padded to the left with spaces.
	pub name: [u8; 8],
	/// DOS date and time (modification time).
	stamp: u32,
	/// List of externally visible symbols defined in this file.
	syms: Vec<String>,
	/// Actual linker object.
	pub cont: ObjectData
}

pub struct Library {
	// Library archive version must be 1.
	// version: u8,
	/// List of files included in the given library.
	pub files: Vec<ObjectFile>
}

pub fn parse(data: &[u8]) -> Result<Library, String> {
	if data.len() < 4 {
		return Err("File too short to be library".to_string());
	}

	let (header, mut rest) = data.split_at(4);

	if header != b"LIB\x01" {
		return Err("Invalid header or library version".to_string());
	}

	let mut out = Library { files: Vec::new() };

	while !rest.is_empty() {
		let (file, _rest) = parse_file(rest)?;
		out.files.push(file);
		rest = _rest;
	}

	Ok(out)
}

fn parse_file(data: &[u8]) -> Result<(ObjectFile, &[u8]), String> {
	if data.len() < 20 {
		return Err("Archived file too short to be object file".to_string());
	}

	let (header, rest) = data.split_at(20);
	let header_len = obtain_le::<u32, _, _, 4>(&header[12..]) as usize;
	let total_len  = obtain_le::<u32, _, _, 4>(&header[16..]) as usize;

	if header_len >= total_len {
		return Err("Header length greater than total length".to_string());
	}

	if header_len <= 20 || rest.len() + 20 < total_len {
		return Err("Archived file was truncated".to_string());
	}

	let (mut raw_syms, rest) = rest.split_at(header_len - 20);
	let (file, rest) = rest.split_at(total_len - header_len);

	let mut syms = Vec::new();

	loop {
		if let Some((&len, _raw_syms)) = raw_syms.split_first() {
			if len == 0 {
				if !_raw_syms.is_empty() {
					return Err("Spurious data in symbol area".to_string());
				}

				break;
			}

			let len = len as usize;

			if _raw_syms.len() < len {
				return Err("Symbol was truncated".to_string());
			}

			let (sym, _raw_syms) = _raw_syms.split_at(len);
			syms.push(String::from_utf8(Vec::from(sym)).map_err(|_| "Invalid symbol data".to_string())?);
			raw_syms = _raw_syms;
		} else {
			return Err("Symbol area was truncated".to_string());
		}
	}

	Ok((ObjectFile {
		name: header[0..8].try_into().unwrap(),
		stamp: obtain_le(&header[8..]),
		syms,
		cont: parse_object(file)?
	}, rest))
}
