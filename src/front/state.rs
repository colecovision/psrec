use std::fmt;

use crate::back::MatcherState;
use super::{
    obj::{ObjectData, Block},
    exe::Executable,
    lib::Library
};

#[derive(Default)]
pub struct ParserState {
    objs: Vec<(String, ObjectData)>,
    secs: Vec<String>,
    exe: Option<(String, Executable)>
}

impl ParserState {
    pub fn has_executable(&self) -> bool {
        self.exe.is_some()
    }

    pub fn parse_executable(&mut self, name: &str, data: &[u8]) -> Result<(), String> {
        Executable::parse(data).map(|exe| {
            self.exe = Some((name.to_string(), exe));
        })
    }

    pub fn parse_object(&mut self, name: &str, data: &[u8]) -> Result<(), String> {
        ObjectData::parse(data).map(|obj| {
            for sec in obj.secs.values() {
                if self.secs.contains(&sec.name) {
                    continue;
                }

                self.secs.push(sec.name.clone());
            }

            self.objs.push((name.to_string(), obj));
        })
    }

    pub fn parse_library(&mut self, name: &str, data: &[u8]) -> Result<(), String> {
        Library::parse(data).map(|lib| {
            for file in lib.files.iter() {
                for sec in file.cont.secs.values() {
                    if self.secs.contains(&sec.name) {
                        continue;
                    }

                    self.secs.push(sec.name.clone());
                }
            }

            self.objs.extend(lib.files.into_iter().map(|file| (
                format!("{}/{}.OBJ", name, std::str::from_utf8(&file.name).unwrap().trim_end()),
                file.cont
            )));
        })
    }

    pub fn into_matcher(self, max_align: u8) -> Option<(MatcherState, Executable)> {
        self.exe.map(|(name, exe)| (
            MatcherState { objs: self.objs, secs: self.secs, name, max_align },
            exe
        ))
    }
}

impl fmt::Display for ParserState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (name, file) in &self.objs {
            writeln!(f, "{}", name)?;

            write!(f, "\tproc: ")?;

            if let Some(proc) = file.proc {
                write!(f, "{}", proc)?;
            } else {
                write!(f, "unspecified")?;
            }

            for file in file.files.values() {
                write!(f, "\n\tsource: {}", file)?;
            }
            for extref in file.refs.values() {
                write!(f, "\n\textref: {}", extref)?;
            }

            for sec in file.secs.values() {
                write!(f, "\n\tsec: {}, size {}", sec.name, sec.blocks.iter().map(Block::len).sum::<usize>()
                                                          + sec.bss.values().map(|b| b.size as usize).sum::<usize>())?;

                for def in sec.defs.values() {
                    write!(f, "\n\t\tdef: {}, off: {}", def.name, def.off)?;
                }

                for bss in sec.bss.values() {
                    write!(f, "\n\t\tbss: {}", bss.name)?;
                }
            }
        }

        Ok(())
    }
}
