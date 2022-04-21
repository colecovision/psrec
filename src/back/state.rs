use crate::front::ObjectData;

use lasso::{Rodeo, RodeoReader};

pub struct MatcherState {
    pub objs: Vec<(String, ObjectData)>,
    pub strings: RodeoReader,
    pub name: String,
    pub max_align: u8
}

impl MatcherState {
    pub fn new(objs: Vec<(String, ObjectData)>, strings: Rodeo, name: String, max_align: u8) -> Self {
        Self {
            objs,
            strings: strings.into_reader(),
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
