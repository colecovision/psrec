use std::ops::Range;

use crate::util::obtain_le;

pub struct Executable {
	_pc0: u32,
	_gp0: u32,
	pub text: (u32, Box<[u8]>),
	// data is always zero
	_bss: Range<u32>,
	_stack: Range<u32>
}

impl Executable {
    pub fn parse(data: &[u8]) -> Result<Self, String> {
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
            _pc0: obtain_le(&head[16..]),
            _gp0: obtain_le(&head[20..]),
            text: (obtain_le(&head[24..]), Box::from(&data[..obtain_le::<u32, 4, _, _>(&head[28..]) as usize])),
            _bss: b_addr .. b_addr + b_size,
            _stack: s_addr .. s_addr + s_size
        })
    }
}
