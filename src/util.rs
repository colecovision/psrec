use std::{borrow::Borrow, ops::Range};

/// A trait made to simplify parsing of binary blobs.
pub trait LeBytes<const N: usize> {
    fn from_le(x: [u8; N]) -> Self;
    fn to_le(self) -> [u8; N];
}

impl LeBytes<4> for u32 {
    fn from_le(x: [u8; 4]) -> Self {
        u32::from_le_bytes(x)
    }

    fn to_le(self) -> [u8; 4] {
        self.to_le_bytes()
    }
}

impl LeBytes<4> for i32 {
    fn from_le(x: [u8; 4]) -> Self {
        i32::from_le_bytes(x)
    }

    fn to_le(self) -> [u8; 4] {
        self.to_le_bytes()
    }
}

impl LeBytes<2> for u16 {
    fn from_le(x: [u8; 2]) -> Self {
        u16::from_le_bytes(x)
    }

    fn to_le(self) -> [u8; 2] {
        self.to_le_bytes()
    }
}

/// More ergonomic way to read `N` bytes from a `&[u8]` slice
/// et simila and get an integer back.
pub fn obtain_le<T, U, V, const N: usize>(x: U) -> T
where T: LeBytes<N>, U: IntoIterator<Item = V>, V: Borrow<u8> {
    let mut y = [0; N];
    y.iter_mut().zip(x).for_each(|(w, r)| *w = *r.borrow());
    <T as LeBytes<N>>::from_le(y)
}



#[derive(Clone, Debug)]
/// Theoretically a diet (discrete interval encoding tree).
/// In practice, the benefits of such a structure are not terribly relevant
/// here, as most instances will rarely hold more than a handful of intervals;
/// the name has stuck. May be replaced by an actual Diet implementation at
/// some point.
pub struct Diet(Vec<(u32, u32)>);

impl Default for Diet {
    fn default() -> Self {
        Self(Vec::new())
    }
}

impl Diet {
    pub fn is_empty(&self) -> bool {
        self.0.len() == 0
    }

    /// The least interval that covers all intervals contained in `self`.
    pub fn radius(&self) -> Range<u32> {
        if let Some(&(l, _)) = self.0.first() {
            l .. self.0.last().unwrap().1
        } else {
            u32::MAX .. 0
        }
    }

    /// Check if interval `elem` is contained in `self`.
    pub fn intersects(&self, elem: (u32, u32)) -> bool {
        match self.0.binary_search_by_key(&elem.0, |x| x.0) {
            Ok(_) => true,
            Err(i) => i < self.0.len() && elem.1 >= self.0[i].0 || i > 0 && elem.0 <= self.0[i - 1].1
        }
    }

    /// Try to insert `elem` in `self`. Returns `true` if all is good, and
    /// `false` if the interval was already present.
    pub fn insert(&mut self, elem: (u32, u32)) -> bool {
        match self.0.binary_search_by_key(&elem.0, |x| x.0) {
            Ok(_) => false,
            Err(i) => {
                if i < self.0.len() && elem.1 >= self.0[i].0 || i > 0 && elem.0 <= self.0[i - 1].1 {
                    false
                } else {
                    let prev = i > 0 && self.0[i - 1].1 + 1 == elem.0;
                    let next = i < self.0.len() && elem.1 + 1 == self.0[i].0;

                    match (prev, next) {
                        (false, false) => self.0.insert(i, elem),
                        (false, true) => self.0[i].0 = elem.0,
                        (true, false) => self.0[i - 1].1 = elem.1,
                        (true, true) => self.0[i - 1].1 = self.0.remove(i).1
                    }

                    true
                }
            }
        }
    }

    /// Check if the two `Self`s are disjoint.
    pub fn is_disjoint(&self, other: &Diet) -> bool {
        let mut i = 0;
        let mut j = 0;

        while i < self.0.len() && j < other.0.len() {
            if self.0[i].1 < other.0[j].0 {
                i += 1;
            } else if other.0[j].1 < self.0[i].0 {
                j += 1;
            } else {
                return false;
            }
        }

        true
    }

    /// Join the two `Self`s into `self`.
    pub fn join(&mut self, other: Diet) {
        // quick and dirty merging

        let mut i = 0;
        let mut j = 0;

        while i < self.0.len() && j < other.0.len() {
            if other.0[j].1 + 1 == self.0[i].0 {
                if i > 0 && self.0[i - 1].1 + 1 == other.0[j].0 {
                    self.0[i - 1].1 = self.0.remove(i).1;
                } else {
                    self.0[i].0 = other.0[j].0;
                    i += 1;
                }

                j += 1;
            } else if other.0[j].1 < self.0[i].0 {
                if i > 0 && self.0[i - 1].1 + 1 == other.0[j].0 {
                    self.0[i - 1].1 = other.0[j].1;
                } else {
                    self.0.insert(i, other.0[j]);
                    i += 1;
                }

                j += 1;
            } else {
                i += 1;
            }
        }

        if j < other.0.len() {
            if let Some(l) = self.0.last_mut() {
                if l.1 + 1 == other.0[j].0 {
                    l.1 = other.0[j].1;
                    j += 1;
                }
            }

            self.0.extend_from_slice(&other.0[j..]);
        }
    }

    /// Get all intervals inside `self` as a slice.
    // Should probably make this into an iterator.
    pub fn ranges(&self) -> &[(u32, u32)] {
        &self.0[..]
    }

    /// Convert `self` to a `Vec` of intervals.
    pub fn as_vec(self) -> Vec<(u32, u32)> {
        self.0
    }
}
