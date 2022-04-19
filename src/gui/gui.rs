use std::{
    collections::{HashMap, HashSet},
    future::ready,
    ops::Range,
    sync::Arc
};

use iced::*;

use crate::{
    OBJS,
    SECS,
    extract::{extract_syms, Instance},
    front::{Executable, ObjectData},
    pattern::LinkPat,
    unif::{UnifyVar, UnifyState}
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

#[derive(Clone, Debug)]
pub enum GuiMessage {
    Extracted(Vec<Instance>),
    InstanceHighlighted(usize),
    InstanceSelected(usize),
    Unhighlight,
    Copy
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

            for sec in &file.secs {
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
                    self.bv.set_status_with(det, Status::revert);
                }
                
                self.bv.set_status_with(det, Status::highlight);
                self.bv.refresh();
            },
            GuiMessage::Unhighlight => {
                if let Some(det) = self.detail.take() {
                    self.bv.set_status_with(det, Status::revert);
                    self.bv.refresh();
                }
            },
            GuiMessage::InstanceSelected(sel) => {
                if self.full.compatible(&self.insts[sel]) {
                    let obj = *self.insts[sel].incl().iter().next().unwrap();

                    self.orphans.extend(unsafe { OBJS[obj].1.refs.iter() }.filter_map(|r| {
                        let real_r = r.name.trim_start_matches('\0');
                        (!self.defined.contains(real_r)).then(|| real_r.to_string())
                    }));

                    for sec in unsafe { &OBJS[obj].1.secs } {
                        sec.defs.iter().for_each(|d| {
                            self.orphans.remove(&d.name);
                        });
                        self.defined.extend(sec.defs.iter().map(|d| d.name.clone()));

                        sec.bss.iter().for_each(|b| {
                            self.orphans.remove(&b.name);
                        });
                        self.defined.extend(sec.bss.iter().map(|b| b.name.clone()));
                    }

                    self.full.join(self.insts[sel].clone());

                    let noncompat = self.insts.iter().enumerate()
                                              .filter_map(|(i, inst)| (!self.full.compatible(inst)).then(|| i))
                                              .collect::<HashSet<_>>();

                    self.bv.set_status(sel, Status::Selected);
                    self.bv.map_status(|pick, col| if matches!(col, Status::Normal | Status::HL) && noncompat.contains(&pick) {
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
            .push(canvas::Canvas::new(&mut self.bv)
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



#[derive(Clone, Copy)]
enum Status {
    Normal,
    HL,
    Selected,
    HLSelected,
    Clashing,
    HLClashing
}

impl Status {
    fn highlight(self) -> Self {
        use Status::*;

        match self {
            Normal => HL,
            Selected => HLSelected,
            Clashing => HLClashing,
            x => x
        }
    }

    fn revert(self) -> Self {
        use Status::*;

        match self {
            HL => Normal,
            HLSelected => Selected,
            HLClashing => Clashing,
            x => x
        }
    }

    fn clash(self) -> Self {
        use Status::*;

        match self {
            Normal | Selected | Clashing => Clashing,
            HL | HLSelected | HLClashing => HLClashing
        }
    }       
}

struct BlockView {
    cache: canvas::Cache,
    ranges: Vec<(usize, usize, Range<u32>, Status, Rectangle<f32>, usize)>,
    block_cols: usize,
    defs: Vec<f32>,
    total: Range<u32>,
    matches: Vec<usize>,
    tot_matches: usize,
    pick: usize,
    start: f32,
    size: f32,
    last: Option<f32>
}

impl BlockView {
    const START_COLOR: Color = Color { r: 0.3, g: 0.7, b: 0.9, a: 1.0 };
    const END_COLOR: Color   = Color { r: 0.9, g: 0.7, b: 0.3, a: 1.0 };
    const ZOOM_SPEED: f32    = 0.9;

    fn new() -> Self {
        Self {
            cache: canvas::Cache::new(),
            ranges: Vec::new(),
            block_cols: 0,
            defs: Vec::new(),
            total: u32::MAX .. 0,
            matches: Vec::new(),
            tot_matches: 0,
            pick: 0,
            start: 0.,
            size: 1.,
            last: None
        }
    }

    fn add(&mut self, inst: &Instance) {
        let id = *inst.incl().iter().next().unwrap();

        if self.matches.len() <= id {
            self.matches.resize(id + 1, usize::MAX);
        }

        if self.matches[id] == usize::MAX {
            self.matches[id] = self.tot_matches;
            self.tot_matches += 1;
        }

        let rad = inst.exts().radius();

        self.total.start = self.total.start.min(rad.start);
        self.total.end = self.total.end.max(rad.end);

        for range in inst.exts().ranges().iter() {
            let ins = match self.ranges.binary_search_by_key(range, |r| (r.2.start, r.2.end)) {
                Ok(pos) => pos + 1,
                Err(pos) => pos
            };

            self.ranges.insert(ins, (id, self.pick, range.0 .. range.1, Status::Normal, Rectangle::default(), 0));
        }

        self.pick += 1;
    }

    fn def(&mut self, pos: u32) {
        let pos = (pos - self.total.start) as f32 / (self.total.end - self.total.start) as f32;

        if let Err(ins) = self.defs.binary_search_by(|x| x.partial_cmp(&pos).unwrap()) {
            self.defs.insert(ins, pos);
        }
    }

    fn map_status<F: Fn(usize, Status) -> Status>(&mut self, f: F) {
        for (_, pick, _, col, _, _) in self.ranges.iter_mut() {
            *col = f(*pick, *col);
        }
    }

    fn set_status_with<F: Fn(Status) -> Status>(&mut self, elem: usize, f: F) {
        self.map_status(|p, c| if p == elem { f(c) } else { c });
    }

    fn set_status(&mut self, elem: usize, status: Status) {
        self.set_status_with(elem, |_| status);
    }

    fn project(&self, pt: Point) -> Option<usize> {
        let hwm = self.ranges.partition_point(|(_, _, _, _, rect, _)| rect.y < pt.y);

        self.ranges[..hwm].iter().find(|(_, _, _, _, rect, _)| rect.contains(pt))
                          .map(|(_, pick, _, _, _, _)| *pick)
    }

    fn recalc(&mut self) {
        if self.total.is_empty() {
            return;
        }

        let mut wait = Vec::new();

        for (_, _, range, _, rect, idx) in &mut self.ranges {
            *idx = match wait.iter().position(|x| x <= &range.start) {
                Some(pos) => {
                    wait[pos] = range.end;
                    pos
                },
                None => {
                    wait.push(range.end);
                    wait.len() - 1
                },
            };

            rect.y = (range.start - self.total.start) as f32 / (self.total.end - self.total.start) as f32;
            rect.height = (range.end - range.start) as f32 / (self.total.end - self.total.start) as f32;
        }
        
        self.block_cols = wait.len();

        for (_, _, _, _, rect, idx) in &mut self.ranges {
            rect.x = (self.block_cols - *idx - 1) as f32 / self.block_cols as f32;
            rect.width = 1.0 / self.block_cols as f32;
        }

        self.refresh();
    }

    fn refresh(&mut self) {
        self.cache.clear();
    }
}

impl canvas::Program<GuiMessage> for BlockView {
    fn draw(&self, bounds: Rectangle<f32>, _cursor: canvas::Cursor) -> Vec<canvas::Geometry> {
        vec![self.cache.draw(bounds.size(), |frame| {
            if self.total.is_empty() {
                return;
            }

            let hwm = self.ranges.partition_point(|(_, _, _, _, rect, _)| rect.y < self.start + self.size);

            for (_, _, _, status, rect, i) in &self.ranges[..hwm] {
                let lo = self.start.max(rect.y);
                let hi = (self.start + self.size).min(rect.y + rect.height);

                let frac_c = (*i as f32 + 0.5) / self.block_cols as f32;
            
                let base_col = Color {
                    r: Self::START_COLOR.r * frac_c + Self::END_COLOR.r * (1.0 - frac_c),
                    g: Self::START_COLOR.g * frac_c + Self::END_COLOR.g * (1.0 - frac_c),
                    b: Self::START_COLOR.b * frac_c + Self::END_COLOR.b * (1.0 - frac_c),
                    a: 1.0
                };

                let col = match status {
                    Status::HL | Status::HLSelected | Status::HLClashing => Color {
                        r: base_col.r * 0.7,
                        g: base_col.g * 0.7,
                        b: base_col.b * 0.7,
                        a: 1.0
                    },
                    Status::Normal => base_col,
                    Status::Selected => base_col.inverse(),
                    Status::Clashing => Color::BLACK
                };

                let tl = Point {
                    x: bounds.x + bounds.width * rect.x,
                    y: bounds.y + bounds.height * (lo - self.start) / self.size
                };

                let sz = Size {
                    width: bounds.width * rect.width,
                    height: bounds.height * (hi - lo) / self.size
                };

                frame.fill_rectangle(tl, sz, col);
            }

            let lwm = self.defs.partition_point(|&x| x < self.start);
            let hwm = self.defs.partition_point(|&x| x <= self.start + self.size);

            for def in &self.defs[lwm..hwm] {
                let def = bounds.y + (def - self.start) * bounds.height / self.size;

                frame.stroke(&canvas::Path::line(
                        Point { x: bounds.x, y: def },
                        Point { x: bounds.x + bounds.width, y: def }
                ), canvas::Stroke::default());
            }
        })]
    }

    fn update(&mut self, event: canvas::Event, bounds: Rectangle<f32>, cursor: canvas::Cursor) -> (canvas::event::Status, Option<GuiMessage>) {
        use canvas::{Event::*, Cursor, event::Status};
        use mouse::{Event::*, ScrollDelta};

        match (event, cursor) {
            (Mouse(WheelScrolled { delta: ScrollDelta::Lines { y, .. } }), Cursor::Available(pt)) if bounds.contains(pt) => {
                let mouse_y = (pt.y - bounds.y) / bounds.height;

                let new_size = (self.size * Self::ZOOM_SPEED.powf(y)).clamp(f32::EPSILON, 1.0);
                let new_start = (self.start + (self.size - new_size) * mouse_y).clamp(0.0, 1.0 - new_size);

                self.size = new_size;
                self.start = new_start;
                self.cache.clear();

                (Status::Captured, None)
            },
            (Mouse(ButtonPressed(mouse::Button::Left)), Cursor::Available(pt)) if bounds.contains(pt) => {
                let click = Point {
                    x: (pt.x - bounds.x) / bounds.width,
                    y: self.start + (pt.y - bounds.y) * self.size / bounds.height
                };

                self.last = Some(click.y);

                (Status::Captured, self.project(click).map(GuiMessage::InstanceHighlighted))
            },
            (Mouse(ButtonPressed(mouse::Button::Right)), Cursor::Available(pt)) if bounds.contains(pt) => {
                let click = Point {
                    x: (pt.x - bounds.x) / bounds.width,
                    y: self.start + (pt.y - bounds.y) * self.size / bounds.height
                };

                (Status::Captured, Some(self.project(click)
                                            .map(GuiMessage::InstanceSelected)
                                            .unwrap_or(GuiMessage::Unhighlight)))
            },
            (Mouse(CursorMoved { .. }), Cursor::Available(pt)) if self.last.is_some() => {
                let mouse_y = self.start + (pt.y - bounds.y) * self.size / bounds.height;
                let last = self.last.unwrap();
                self.start = (self.start + last - mouse_y).clamp(0.0, 1.0 - self.size);
                self.cache.clear();
                (Status::Captured, None)
            },
            (Mouse(ButtonReleased(mouse::Button::Left)), _) if self.last.is_some() => {
                self.last = None;
                (Status::Captured, None)
            },
            _ => (Status::Ignored, None)
        }
    }
}
