use crate::front::ObjectData;

use lasso::{Rodeo, RodeoReader};

pub struct MatcherState {
    pub objs: Vec<(String, ObjectData)>,
    pub secs: RodeoReader,
    pub name: String,
    pub max_align: u8
}

impl MatcherState {
    pub fn new(objs: Vec<(String, ObjectData)>, name: String, max_align: u8) -> Self {
        let mut rodeo = Rodeo::new();

        for (_, obj) in &objs {
            for sec in obj.secs.values() {
                rodeo.get_or_intern(&sec.name);
            }
        }

        Self {
            objs,
            secs: rodeo.into_reader(),
            name,
            max_align
        }
    }

    pub fn obj_name(&self, idx: usize) -> &str {
        &self.objs[idx].0
    }

    pub fn obj(&self, idx: usize) -> &ObjectData {
        &self.objs[idx].1
    }

    pub fn num_objs(&self) -> usize {
        self.objs.len()
    }
}
