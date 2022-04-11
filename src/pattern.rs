use std::cmp::Ordering;

use crate::front::{Block, Section, PatchKind};

#[derive(Debug, Clone, Copy)]
pub struct Mask {
    pub data: u8,
    pub mask: u8
}

impl PartialEq for Mask {
    fn eq(&self, other: &Self) -> bool {
        self.mask == other.mask && self.data & self.mask == other.data & other.mask
    }
}

impl Eq for Mask {}

impl PartialOrd for Mask {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        // self <= other if whenever self matches, other matches as well.

        let combi = self.mask & other.mask;

        if self.data & combi == other.data & combi {
            if self.mask == other.mask {
                return Some(Ordering::Equal);
            } else if combi == self.mask {
                return Some(Ordering::Greater);
            } else if combi == other.mask {
                return Some(Ordering::Less);
            }
        }

        None
    }
}

impl PartialEq<Mask> for u8 {
    fn eq(&self, other: &Mask) -> bool {
        other.mask == 0xFF && other.data == *self
    }
}

impl PartialOrd<Mask> for u8 {
    fn partial_cmp(&self, other: &Mask) -> Option<Ordering> {
        (Mask { data: *self, mask: 0xFF }).partial_cmp(other)
    }
}

#[derive(Debug)]
pub struct LinkPat {
    pat: Box<[Mask]>,
    kmp: Box<[isize]>
}

impl LinkPat {
    pub fn len(&self) -> usize {
        self.pat.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn data(&self) -> &[Mask] {
        &self.pat
    }

    pub fn section(sec: &Section, max_align: u8) -> Self {
        let patlen = sec.blocks.iter().map(Block::len).sum::<usize>()
                   + sec.bss.iter().map(|b| b.size as usize).sum::<usize>();
        let mut pat = Vec::with_capacity(patlen);

        for block in &sec.blocks {
            match block {
                Block::Code(b) => pat.extend(b.iter().map(|&data| Mask { data, mask: 0xFF })),
                Block::Uninit(x) => pat.resize(pat.len() + *x as usize, Mask { data: 0, mask: 0 })
            }
        }

        for patch in &sec.patches {
            let off = patch.off as usize;

            pat[off].mask = 0;
            pat[off + 1].mask = 0;

            match patch.kind {
                PatchKind::Low28 => {
                    pat[off + 2].mask = 0;
                    pat[off + 3].mask &= 0xFC;
                },
                PatchKind::Full => {
                    pat[off + 2].mask = 0;
                    pat[off + 3].mask = 0;
                },
                PatchKind::Upper16 | PatchKind::Lower16 => ()
            }
        }

        let mut kmp = Vec::with_capacity(patlen);
        let align = sec.align.min(max_align) as isize;
        kmp.extend(-align .. (pat.len() as isize + 1 - align).min(0));

        for i in align .. pat.len() as isize + 1 {
            let mut z = i;

            kmp.push(loop {
                z = kmp[(z - align) as usize] + align;

                if z <= 0 || pat[(i - align) as usize .. i as usize]
                                .iter()
                                .zip(pat[z as usize .. (z + align) as usize].iter())
                                .all(|(x, y)| x <= y) {
                    break z;
                }
            });
        }

        Self {
            pat: pat.into_boxed_slice(),
            kmp: kmp.into_boxed_slice()
        }
    }

    pub fn usable(&self) -> bool {
        self.pat.iter().any(|x| x.mask != 0)
    }

    pub fn find<'a, 'b, T: PartialOrd<Mask>>(&'a self, data: &'b [T]) -> FindIter<'a, 'b, T> {
        FindIter {
            pat: &self,
            data,
            i: 0,
            j: 0
        }
    }
}

pub struct FindIter<'a, 'b, T: PartialOrd<Mask>> {
    pat: &'a LinkPat,
    data: &'b [T],
    i: usize, // init 0
    j: usize // init 0
}

impl<T: PartialOrd<Mask>> Iterator for FindIter<'_, '_, T> {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        while let Some(this) = self.data.get(self.i) {
            let dep = this <= &self.pat.pat[self.j];

            if dep {
                self.j += 1;
                self.i += 1;

                if self.j == self.pat.pat.len() {
                    let out = self.i - self.pat.pat.len();
                    self.i = self.i.wrapping_sub(self.pat.kmp[self.j].min(0) as usize);
                    self.j = self.pat.kmp[self.j].max(0) as usize;
                    return Some(out);
                }
            } else {
                self.i = self.i.wrapping_sub(self.pat.kmp[self.j].min(0) as usize);
                self.j = self.pat.kmp[self.j].max(0) as usize;
            }
        }

        None
    }
}

use std::iter::FusedIterator;

impl<T: PartialOrd<Mask>> FusedIterator for FindIter<'_, '_, T> {}
