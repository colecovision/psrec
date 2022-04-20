use std::collections::{HashMap, HashSet};

use crate::{
    front::{ObjectData, Section},
    pattern::LinkPat,
    unif::{UnifyVar, UnifyState},
    util::{Diet, obtain_le}
};

pub fn extract_syms<F: Fn(&str) -> usize>(
    offset: u32,
    actual: &[u8], pat: &LinkPat,
    sec: &Section, file: &ObjectData,
    id: usize, name: usize,
    sec_tl: F
) -> Option<((u32, u32), HashMap<UnifyVar, UnifyState>)> {
    let mut syms = HashMap::new();
    let ext = (offset, offset + pat.len() as u32 - 1);

    syms.insert(UnifyVar::SecBase(name, id), UnifyState::InRange(offset, 0));

    for def in sec.defs.values() {
        syms.insert(UnifyVar::Symbol(def.name.clone()), UnifyState::InRange(offset + def.off, 0));
    }

    for patch in &sec.patches {
        let off = patch.off as usize;
        // let q: u32 = obtain_le(pat.data()[off..].iter().map(|x| x.data));
        let w: u32 = obtain_le(&actual[off..]);

        let (var, off) = patch.expr.as_var(name, file, &sec_tl);
        let val = patch.kind.as_state(w, offset + patch.off as u32) - off;

        let sv = syms.entry(var).or_insert(val);
        *sv = sv.unify(val)?;
    }

    Some((ext, syms))
}

#[derive(Clone, Debug)]
pub struct Instance {
    ext: Diet,
    incl: HashSet<usize>,
    syms: HashMap<UnifyVar, UnifyState>
}

impl Instance {
    pub fn empty() -> Self {
        Self::new(HashSet::new())
    }

    pub fn new(incl: HashSet<usize>) -> Self {
        Self {
            ext: Diet::default(),
            incl,
            syms: HashMap::new()
        }
    }

    pub fn exts(&self) -> &Diet {
        &self.ext
    }

    pub fn incl(&self) -> &HashSet<usize> {
        &self.incl
    }

    pub fn syms(&self) -> &HashMap<UnifyVar, UnifyState> {
        &self.syms
    }

    pub fn insert(&mut self, ext: (u32, u32), syms: HashMap<UnifyVar, UnifyState>) -> bool {
        for (k, v) in syms {
            let uv = self.syms.entry(k).or_insert(v.clone());

            if let Some(v) = uv.unify(v) {
                *uv = v;
            } else {
                return false;
            }
        }

        self.ext.insert(ext)
    }

    pub fn compatible(&self, other: &Instance) -> bool {
        if !self.incl.is_disjoint(&other.incl) {
            return false;
        }

        if !self.ext.is_disjoint(&other.ext) {
            return false;
        }

        if self.syms.iter().any(|(k, v)| (*other.syms.get(k).unwrap_or(v)).unify(*v).is_none()) {
            return false;
        }
        
        true
    }

    pub fn join(&mut self, other: Instance) {
        let Instance { incl, ext, syms } = other;

        for (k, v) in syms {
            self.syms.entry(k.clone()).and_modify(|vv| *vv = vv.unify(v).unwrap())
                                      .or_insert(v);
        }

        for file in incl {
            self.incl.insert(file);
        }

        self.ext.join(ext);
    }
}
