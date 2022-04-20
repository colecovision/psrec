use crate::front::ObjectData;

pub struct MatcherState {
    pub objs: Vec<(String, ObjectData)>,
    pub secs: Vec<String>,
    pub name: String,
    pub max_align: u8
}

impl MatcherState {
    pub fn sec_idx(&self, name: &str) -> Option<usize> {
        self.secs.iter().position(|x| x == name)
    }

    pub fn sec_name(&self, idx: usize) -> &str {
        &self.secs[idx]
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
