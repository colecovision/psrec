use std::{fs, env};

use iced::{Application, Settings};

mod front;
mod back;
mod util;
mod pattern;
mod unif;
mod extract;
mod gui;
use front::ParserState;
use gui::MainGui;

pub fn main() -> iced::Result {
    let mut args = env::args().skip(1);
    let mut max_align = u8::MAX;
    let mut no_dump = false;

    let mut state = ParserState::default();

    while let Some(name) = args.next() {
        match name.as_str() {
            "-x" | "--executable" => {
                if state.has_executable() {
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

                match state.parse_executable(&name, &data) {
                    Ok(()) => println!("{} parsed OK!", name),
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

                match state.parse_object(&name, &data) {
                    Ok(()) => println!("{} parsed OK!", name),
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

                match state.parse_library(&name, &data) {
                    Ok(()) => println!("{} parsed OK!", name),
                    Err(e) => println!("Error while parsing {}: {}", name, e)
                }
            }
        }
    }

    if !no_dump {
        println!("{}", state);
    }

    state.into_matcher(max_align).map_or(
        Ok(()),
        |se| MainGui::run(Settings::with_flags(se))
    )
}
