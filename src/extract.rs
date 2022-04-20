use std::collections::{HashMap, HashSet};

use crate::{
    front::{ObjectData, Section, Expr, PatchKind},
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

    syms.insert(UnifyVar::Section(name, id), UnifyState::InRange(offset, 0));

    for def in sec.defs.values() {
        syms.insert(UnifyVar::Symbol(def.name.clone()), UnifyState::InRange(offset + def.off, 0));
    }

    for patch in &sec.patches {
        let off = patch.off as usize;
        let q: u32 = obtain_le(pat.data()[off..].iter().map(|x| x.data));
        let w: u32 = obtain_le(&actual[off..]);

        let (var, off) = match patch.expr {
            Expr::SectionPlus(idx, off) => {
                (UnifyVar::Section(name, sec_tl(&file.sec_by_idx(idx).unwrap().name)), off)
            },
            Expr::SymbolPlus(idx, off) => {
                (UnifyVar::Symbol(file.sym_name_from_idx(idx).unwrap().to_string()), off)
            },
            Expr::SectionStart(idx) => {
                (UnifyVar::SecStart(sec_tl(&file.sec_by_idx(idx).unwrap().name)), 0)
            },
            Expr::SectionBytes(idx) => {
                (UnifyVar::SecSizeBytes(sec_tl(&file.sec_by_idx(idx).unwrap().name)), 0)
            },
            Expr::SectionEnd(idx) => {
                (UnifyVar::SecEnd(sec_tl(&file.sec_by_idx(idx).unwrap().name)), 0)
            },
            _ => unimplemented!("what expr: {:?} for {:08X} => {:08X}; with offset {} ({:08X}) of {}/{}", patch.expr, q, w, patch.off, offset, name, id)
        };

        let val = match patch.kind {
            PatchKind::Low28 => {
                // eprintln!("patching {:?} at offset {} ({:08X}) with expr {:?}; original is {:08X}, found is {:08X}", patch.kind, patch.off, offset, patch.expr, q, w);
                UnifyState::InRange((w << 2 & 0xFFFFFFC | offset + patch.off as u32 & 0xF0000000).wrapping_sub(off), 0)
            },
            PatchKind::Full => {
                // eprintln!("patching {:?} at offset {} of {}/{} with expr {:?}; original is {:08X}, found is {:08X}", patch.kind, patch.off, name, sec.name, patch.expr, q, w);
                UnifyState::InRange(w.wrapping_sub(off), 0)
            },
            PatchKind::Upper16 => {
                // eprintln!("patching {:?} at offset {} with expr {:?}; original is {:08X}, found is {:08X}", patch.kind, patch.off, patch.expr, q, w);
                UnifyState::InRange((w << 16).wrapping_sub(off).wrapping_sub(32768), 65535)
            },
            PatchKind::Lower16 => {
                // eprintln!("patching {:?} at offset {} with expr {:?}; original is {:08X}, found is {:08X}", patch.kind, patch.off, patch.expr, q, w);
                UnifyState::Lower16((w as u16).wrapping_sub(off as u16))
            }
        };

        // eprintln!("{:?} =?= {:?}", var, val);

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
