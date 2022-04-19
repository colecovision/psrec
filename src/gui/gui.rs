use std::{
    collections::{HashMap, HashSet},
    future::ready,
    sync::Arc
};

use iced::{
    button,
    executor,
    scrollable,

    Application, Button, Canvas,
    Clipboard, Color, Column,
    Command, Element, HorizontalAlignment,
    Length, Row, Scrollable,
    Text
};

use crate::{
    OBJS,
    SECS,
    extract::{extract_syms, Instance},
    front::{Executable, ObjectData},
    pattern::LinkPat,
    unif::{UnifyVar, UnifyState}
};
use super::{
    GuiMessage,
    view::{BlockView, State}
};

pub struct MainGui {
    name: String,
    insts: Vec<Instance>,
    bv: BlockView,
    detail: Option<usize>,
    full: Instance,
    defined: HashSet<String>,
    orphans: HashSet<String>,
    scroll: scrollable::State,
    button: button::State
}

fn instance_repr(inst: &Instance) -> String {
    let ranges = inst.exts().ranges().iter()
                     .map(|(s, e)| format!("\n∙ 0x{:08X}—0x{:08X}", s, e))
                     .collect::<String>();

    let incls = inst.incl().iter()
                    .map(|&x| format!("\n∙ {}", unsafe { &OBJS[x].0 }))
                    .collect::<String>();

    let mut syms = inst.syms().iter().map(|(uv, us)| format!("{} {}",
        match us {
            UnifyState::InRange(x, 0) => format!("0x{:08X}", x),
            UnifyState::InRange(x, y) => format!("0x{:08X}–0x{:08X}", x, x + *y as u32),
            UnifyState::Lower16(y) => format!("0x????{:04X}", y)
        },
        match uv {
            UnifyVar::Symbol(s) => s.clone(),
            &UnifyVar::Section(obj, sec) => format!("{}/{}", unsafe { &OBJS[obj].0 }, unsafe { &SECS[sec] }),
            &UnifyVar::SecStart(sec) => format!("_START({})", unsafe { &SECS[sec] }),
            &UnifyVar::SecSizeBytes(sec) => format!("_SIZEOF({})", unsafe { &SECS[sec] })
        },
    )).collect::<Vec<_>>();
    syms.sort();

    format!("ranges:{}\nobject files:{}\nsymbols:\n∙ {}", ranges, incls, syms.join("\n∙ "))
}

impl Application for MainGui {
    type Executor = executor::Default;
    type Message = GuiMessage;
    type Flags = (String, Executable, u8);

    fn new(flags: Self::Flags) -> (Self, Command<Self::Message>) {
        let exe = Arc::new(flags.1);
        let out = Self {
            name: flags.0,
            insts: Vec::new(),
            bv: BlockView::new(),
            detail: None,
            full: Instance::empty(),
            defined: HashSet::new(),
            orphans: HashSet::new(),
            scroll: scrollable::State::new(),
            button: button::State::new()
        };

        let run = move |(i, file): (usize, &ObjectData)| {
            let mut poss = Vec::with_capacity(1);
            let mut check_later = HashSet::new();
            let mut checked = HashSet::new();
            let mut incl = HashSet::new();

            incl.insert(i);
            poss.push(Instance::new(incl));

            let sec_tl = |s: &str| unsafe { SECS.iter() }.position(|x| x == s)
                                                         .unwrap();

            for sec in file.secs.values() {
                let pat = LinkPat::section(sec, flags.2);
                let id = sec_tl(&sec.name);

                if !pat.usable() {
                    if !pat.is_empty() {
                        check_later.insert(id);
                    }

                    continue;
                }

                let mut new_poss = Vec::new();

                for (e, s) in pat.find(&exe.text.1).filter_map(
                    |pos| extract_syms(exe.text.0 + pos as u32,
                                       &exe.text.1[pos..pos + pat.len()],
                                       &pat, sec, file, id, i, sec_tl)
                ) {
                    for ins in &poss {
                        let mut ins = ins.clone();

                        if ins.insert(e, s.clone()) {
                            new_poss.push(ins);
                        }
                    }
                }

                poss = new_poss;

                if poss.is_empty() {
                    return GuiMessage::Extracted(poss);
                }
            }

            if poss.len() == 1 && poss[0].exts().is_empty() {
                poss.clear();
                return GuiMessage::Extracted(poss);
            }

            println!("object {}: {} instances; check {}", i, poss.len(),
                     check_later.iter().copied().map(|x| format!("{}", x))
                                       .collect::<Vec<_>>().join(" "));

            for sec_check in check_later.iter().copied().cycle() {
                if check_later == checked {
                    break;
                }

                checked.insert(sec_check);

                let mut new_poss = Vec::new();

                for inst in &poss {
                    if let Some(pos) = inst.syms().get(&UnifyVar::Section(i, sec_check)) {
                        let pos = match pos {
                            &UnifyState::InRange(x, 0) => x,
                            _ => unimplemented!("fuzzy rescan")
                        };

                        let sec = file.sec_by_name(unsafe { &SECS[sec_check] }).unwrap();
                        let pat = LinkPat::section(sec, flags.2);
                        let off = (pos - exe.text.0) as usize;

                        if off + pat.len() < exe.text.1.len() {
                            if let Some((e, s)) = extract_syms(pos, &exe.text.1[off .. off + pat.len()], &pat, sec, file, sec_check, i, sec_tl) {
                                let mut ins = inst.clone();

                                if ins.insert(e, s) {
                                    new_poss.push(ins);
                                }
                            }
                        }
                    } else {
                        new_poss.push(inst.clone());
                        checked.remove(&sec_check);
                    }
                }

                poss = new_poss;

                if poss.is_empty() {
                    return GuiMessage::Extracted(poss);
                }
            }

            let mut secdata = HashMap::<usize, u32>::new();
            let mut advertised = HashMap::<usize, u32>::new();

            poss.retain(|inst| {
                for (k, v) in inst.syms() {
                    match k {
                        UnifyVar::Symbol(_) => (),
                        UnifyVar::Section(_, name) => {
                            if let UnifyState::InRange(val, 0) = v {
                                secdata.insert(*name, *val);
                            }
                        },
                        UnifyVar::SecStart(name) => {
                            if let UnifyState::InRange(val, 0) = v {
                                advertised.insert(*name, *val);
                            }
                        },
                        UnifyVar::SecSizeBytes(_) => unimplemented!("check secsizebytes")
                    }
                }

                advertised.iter().all(|(&sec, &min)| min <= secdata.get(&sec).copied().unwrap_or(u32::MAX))
            });

            GuiMessage::Extracted(poss)
        };

        (out, Command::batch((0..unsafe { OBJS.len() }).map(
            |i| Command::perform(ready((i, unsafe { &OBJS[i].1 })), run.clone())
        )))
    }

    fn title(&self) -> String {
        format!("psrec - {}", self.name)
    }

    fn update(&mut self, msg: Self::Message, clip: &mut Clipboard) -> Command<Self::Message> {
        match msg {
            GuiMessage::Extracted(mut poss) => {
                for inst in &poss {
                    self.bv.add(inst);
                }

                self.bv.recalc();
                self.insts.append(&mut poss);
            },
            GuiMessage::InstanceHighlighted(det) => {
                if let Some(det) = std::mem::replace(&mut self.detail, Some(det)) {
                    self.bv.set_state_with(det, State::revert);
                }
                
                self.bv.set_state_with(det, State::highlight);
                self.bv.refresh();
            },
            GuiMessage::Unhighlight => {
                if let Some(det) = self.detail.take() {
                    self.bv.set_state_with(det, State::revert);
                    self.bv.refresh();
                }
            },
            GuiMessage::InstanceSelected(sel) => {
                if self.full.compatible(&self.insts[sel]) {
                    let obj = *self.insts[sel].incl().iter().next().unwrap();

                    self.orphans.extend(unsafe { OBJS[obj].1.refs.values() }.filter_map(|r| {
                        let real_r = r.trim_start_matches('\0');
                        (!self.defined.contains(real_r)).then(|| real_r.to_string())
                    }));

                    for sec in unsafe { &OBJS[obj].1.secs }.values() {
                        sec.defs.values().for_each(|d| {
                            self.orphans.remove(&d.name);
                        });
                        self.defined.extend(sec.defs.values().map(|d| d.name.clone()));

                        sec.bss.values().for_each(|b| {
                            self.orphans.remove(&b.name);
                        });
                        self.defined.extend(sec.bss.values().map(|b| b.name.clone()));
                    }

                    self.full.join(self.insts[sel].clone());

                    let noncompat = self.insts.iter().enumerate()
                                              .filter_map(|(i, inst)| (!self.full.compatible(inst)).then(|| i))
                                              .collect::<HashSet<_>>();

                    self.bv.set_state(sel, State::Selected);
                    self.bv.map_state(|pick, col| if matches!(col, State::Normal | State::HL) && noncompat.contains(&pick) {
                        col.clash()
                    } else {
                        col
                    });

                    for (_, us) in self.full.syms().iter() {
                        if let &UnifyState::InRange(x, 0) = us {
                            self.bv.def(x);
                        }
                    }

                    self.bv.refresh();
                }
            },
            GuiMessage::Copy => {
                clip.write(instance_repr(if let Some(det) = self.detail {
                    &self.insts[det]
                } else {
                    &self.full
                }));
            }
        }

        Command::none()
    }

    fn view(&mut self) -> Element<Self::Message> {
        let s = if let Some(det) = self.detail {
            instance_repr(&self.insts[det])
        } else {
            format!("== global selection ==\n{}", instance_repr(&self.full))
        };

        let orph = self.orphans.iter().map(|s| format!(" {}", s)).collect::<String>();

        Row::new()
            .push(Canvas::new(&mut self.bv)
                         .width(Length::Fill)
                         .height(Length::Fill))
            .push(Column::new()
                         .push(Scrollable::new(&mut self.scroll)
                                          .push(Text::new(format!("orphans:{}", orph))
                                                     .color(Color { r: 0.8, g: 0.0, b: 0.1, a: 1.0 }))
                                          .push(Text::new(s))
                                          .width(Length::Fill)
                                          .height(Length::Fill))
                         .push(Button::new(&mut self.button, Text::new("Copy to clipboard")
                                                                  .horizontal_alignment(HorizontalAlignment::Center))
                                      .on_press(GuiMessage::Copy)
                                      .width(Length::Fill)
                                      .height(Length::Shrink))
                         .width(Length::Fill)
                         .height(Length::Fill))
            .into()
    }
}
