use std::{fs, env};

use iced::{Application, Settings};

mod front;
mod util;
mod pattern;
mod unif;
mod extract;
mod gui;
use front::{Library, Processor, Executable, ObjectData, Block};
use gui::MainGui;


static mut OBJS: Vec<(String, ObjectData)> = Vec::new();
static mut SECS: Vec<String> = Vec::new();


pub fn main() -> iced::Result {
    let mut exec = None;
    let mut args = env::args().skip(1);
    let mut max_align = u8::MAX;
    let mut no_dump = false;

    while let Some(name) = args.next() {
        match name.as_str() {
            "-x" | "--executable" => {
                if exec.is_some() {
                    eprintln!("Two executables provided, using the latter.");
                }

                let name = match args.next() {
                    Some(name) => name,
                    None => {
                        eprintln!("Error: no executable given");
                        continue;
                    }
                };

                let data = match fs::read(&name) {
                    Ok(data) => data,
                    Err(e) => {
                        eprintln!("Error while opening {}: {}", name, e);
                        continue;
                    }
                };

                match Executable::parse(&data) {
                    Ok(exe) => {
                        println!("{} parsed OK!", name);
                        exec = Some((name, exe));
                    },
                    Err(e) => println!("Error while parsing {}: {}", name, e)
                }
            },

            "-o" | "--object" => {
                let name = match args.next() {
                    Some(name) => name,
                    None => {
                        eprintln!("Error: no executable given");
                        continue;
                    }
                };

                let data = match fs::read(&name) {
                    Ok(data) => data,
                    Err(e) => {
                        eprintln!("Error while opening {}: {}", name, e);
                        continue;
                    }
                };

                match ObjectData::parse(&data) {
                    Ok(obj) => {
                        println!("{} parsed OK!", name);
                        unsafe { OBJS.push((name, obj)); }
	                },
                    Err(e) => println!("Error while parsing {}: {}", name, e)
                }
            },

            "-d" | "--no-dump" => {
                no_dump = true;
            },

            "-f" | "--force-align" => {
                max_align = 4;
            },

            _ => {
                let data = match fs::read(&name) {
                    Ok(data) => data,
                    Err(e) => {
                        eprintln!("Error while opening {}: {}", name, e);
                        continue;
                    }
                };

                match Library::parse(&data) {
                    Ok(lib) => {
                        println!("{} parsed OK!", name);

                        unsafe { OBJS.extend(lib.files.into_iter()
                                             .map(|file| (
                            format!("{}/{}.OBJ", name, std::str::from_utf8(&file.name).unwrap().trim_end()),
                            file.cont
                        ))); }
                    },
                    Err(e) => println!("Error while parsing {}: {}", name, e)
                }
            }
        }
    }

    if !no_dump {
        for (name, file) in unsafe { OBJS.iter() } {
            println!("\t{}", name);

            println!("\t\tproc: {}", match file.proc {
                None => "unspecified",
                Some(Processor::M68K) => "Motorola 68000 family",
                Some(Processor::WDC65816) => "WDC 65816",
                Some(Processor::MOS6502) => "MOS 6502",
                Some(Processor::Z80) => "ZiLOG Z80",
                Some(Processor::SPC700) => "Sony SPC-700",
                Some(Processor::X86) => "Intel x86 family",
                Some(Processor::ARM) => "ARM family",
                Some(Processor::R3K) => "MIPS R3000",
                Some(Processor::SH) => "Hitachi SuperH family",
                Some(Processor::R4K5K) => "MIPS R4000 and R5000"
            });

            for file in file.files.values() {
                println!("\t\tsource: {}", file);
            }

            for extref in file.refs.values() {
                println!("\t\textref: {}", extref);
            }

            for sec in file.secs.values() {
                println!("\t\tsec: {}, size {}", sec.name, sec.blocks.iter().map(Block::len).sum::<usize>()
                                                         + sec.bss.iter().map(|b| b.size as usize).sum::<usize>());

                for def in &sec.defs {
                    println!("\t\t\tdef: {}, off: {}", def.name, def.off);
                }

                for bss in &sec.bss {
                    println!("\t\t\tbss: {}", bss.name);
                }
            }
        }
    }

    if let Some((name, exe)) = exec {
        for (_, file) in unsafe { OBJS.iter() } {
            for sec in file.secs.values() {
                if unsafe { SECS.contains(&sec.name) } {
                    continue;
                }

                unsafe { SECS.push(sec.name.clone()); }
            }
        }

        MainGui::run(Settings::with_flags((name, exe, max_align)))
    } else {
        Ok(())
    }
}
