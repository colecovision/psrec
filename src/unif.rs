#[derive(PartialEq, Eq, Hash, Clone, Debug)]
/// A symbolic variable whose value may be derived from either
/// 1. the executable file structure or
/// 2. data stored inside the executable.
pub enum UnifyVar {
    /// A symbol, as identified by its name.
    Symbol(String),
    /// A section of a specific object file, as identified by its two indices
    /// (the former being the object file index and the latter the section
    /// index).
    Section(usize, usize),
    /// The start of a section in the executable, as identified by its index.
    SecStart(usize),
    /// The size of a section in the executable, as identified by its index.
    SecSizeBytes(usize),
    /// The end of a section in the executable, as identified by its index.
    SecEnd(usize)
}

#[derive(Copy, Clone, Debug)]
/// The degree of knowledge about the value of a symbolic variable.
pub enum UnifyState {
    /// The value is in the (possibly overflowing) inclusive interval
    /// starting at the first element, of length equal to the second.
    InRange(u32, u16),
    /// The value is known only modulo 65536.
    Lower16(u16)
}

impl UnifyState {
    /// Attempts to merge knowledge about some variable.
    /// On success, returns a `Some` containing the merged set.
    /// On failure, returns `None`.
    pub fn unify(self, other: Self) -> Option<Self> {
        use UnifyState::*;

        match (self, other) {
            (InRange(l1, x1), InRange(l2, x2)) => {
                let (l1, l2, x1, x2) = if x1 >= x2 {
                    (l1, l2, x1, x2)
                } else {
                    (l2, l1, x2, x1)
                };

                let delta = l2.wrapping_sub(l1);

                if delta <= x1 as u32 {
                    Some(InRange(l2, x2.min(x1 - delta as u16)))
                } else if delta.wrapping_add(x2 as u32) <= x1 as u32 {
                    Some(InRange(l1, x2.wrapping_add(delta as u16)))
                } else {
                    None
                }
            },
            // modulo is a congruence!
            (Lower16(x), Lower16(y)) => (x == y).then(|| Lower16(x)),
            // interval + modulo = full information IF COMPATIBLE
            (InRange(l, x), Lower16(y)) | (Lower16(y), InRange(l, x)) => {
                let delta = y.wrapping_sub(l as u16);

                if delta <= x {
                    Some(InRange(l.wrapping_add(delta as u32), 0))
                } else {
                    None
                }
            }
        }
    }
}
