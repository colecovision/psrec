use super::obj::ObjectData;
use crate::util::obtain_le;

pub struct Library {
	// Library archive version must be 1.
	// version: u8,
	/// List of files included in the given library.
	pub files: Vec<ObjectFile>
}

impl Library {
	pub fn parse(data: &[u8]) -> Result<Self, String> {
		if data.len() < 4 {
			return Err("File too short to be library".to_string());
		}

		let (header, mut rest) = data.split_at(4);

		if header != b"LIB\x01" {
			return Err("Invalid header or library version".to_string());
		}

		let mut out = Self { files: Vec::new() };

		while !rest.is_empty() {
			let (file, _rest) = ObjectFile::parse(rest)?;
			out.files.push(file);
			rest = _rest;
		}

		Ok(out)
	}
}

pub struct ObjectFile {
	/// File name; padded to the left with spaces.
	pub name: [u8; 8],
	/// DOS date and time (modification time).
	_stamp: u32,
	/// List of externally visible symbols defined in this file.
	_syms: Vec<String>,
	/// Actual linker object.
	pub cont: ObjectData
}

impl ObjectFile {
	fn parse(data: &[u8]) -> Result<(Self, &[u8]), String> {
		if data.len() < 20 {
			return Err("Archived file too short to be object file".to_string());
		}

		let (header, rest) = data.split_at(20);
		let header_len = obtain_le::<u32, 4, _, _>(&header[12..]) as usize;
		let total_len  = obtain_le::<u32, 4, _, _>(&header[16..]) as usize;

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
			_stamp: obtain_le(&header[8..]),
			_syms: syms,
			cont: ObjectData::parse(file)?
		}, rest))
	}
}
