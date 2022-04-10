#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum UnifyVar {
    Symbol(String),
    Section(usize, usize),
    SecStart(usize),
    SecSizeBytes(usize),
}

#[derive(Copy, Clone, Debug)]
pub enum UnifyState {
    InRange(u32, u16), // x.0 <= VALUE <= x.0 + x.1
    Lower16(u16)       // VALUE mod 65536 = x.0
}

impl UnifyState {
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
            (Lower16(x), Lower16(y)) => (x == y).then(|| Lower16(x)),
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
