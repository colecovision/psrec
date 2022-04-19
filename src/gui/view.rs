use std::ops::Range;

use iced::{
    canvas::{
        event::Status,
        Cache, Cursor, Event,
        Geometry, Path, Program,
        Stroke
    },
    mouse,

    Color, Point, Rectangle,
    Size
};

use crate::extract::Instance;
use super::GuiMessage;

#[derive(Clone, Copy)]
pub enum State {
    Normal,
    HL,
    Selected,
    HLSelected,
    Clashing,
    HLClashing
}

impl State {
    pub fn highlight(self) -> Self {
        use State::*;

        match self {
            Normal => HL,
            Selected => HLSelected,
            Clashing => HLClashing,
            x => x
        }
    }

    pub fn revert(self) -> Self {
        use State::*;

        match self {
            HL => Normal,
            HLSelected => Selected,
            HLClashing => Clashing,
            x => x
        }
    }

    pub fn clash(self) -> Self {
        use State::*;

        match self {
            Normal | Selected | Clashing => Clashing,
            HL | HLSelected | HLClashing => HLClashing
        }
    }       
}

pub(super) struct BlockView {
    cache: Cache,
    ranges: Vec<(usize, usize, Range<u32>, State, Rectangle<f32>, usize)>,
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

    pub fn new() -> Self {
        Self {
            cache: Cache::new(),
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

    pub fn add(&mut self, inst: &Instance) {
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

            self.ranges.insert(ins, (id, self.pick, range.0 .. range.1, State::Normal, Rectangle::default(), 0));
        }

        self.pick += 1;
    }

    pub fn def(&mut self, pos: u32) {
        let pos = (pos - self.total.start) as f32 / (self.total.end - self.total.start) as f32;

        if let Err(ins) = self.defs.binary_search_by(|x| x.partial_cmp(&pos).unwrap()) {
            self.defs.insert(ins, pos);
        }
    }

    pub fn map_state<F: Fn(usize, State) -> State>(&mut self, f: F) {
        for (_, pick, _, col, _, _) in self.ranges.iter_mut() {
            *col = f(*pick, *col);
        }
    }

    pub fn set_state_with<F: Fn(State) -> State>(&mut self, elem: usize, f: F) {
        self.map_state(|p, c| if p == elem { f(c) } else { c });
    }

    pub fn set_state(&mut self, elem: usize, status: State) {
        self.set_state_with(elem, |_| status);
    }

    fn project(&self, pt: Point) -> Option<usize> {
        let hwm = self.ranges.partition_point(|(_, _, _, _, rect, _)| rect.y < pt.y);

        self.ranges[..hwm].iter().find(|(_, _, _, _, rect, _)| rect.contains(pt))
                          .map(|(_, pick, _, _, _, _)| *pick)
    }

    pub fn recalc(&mut self) {
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

    pub fn refresh(&mut self) {
        self.cache.clear();
    }
}

impl Program<GuiMessage> for BlockView {
    fn draw(&self, bounds: Rectangle<f32>, _cursor: Cursor) -> Vec<Geometry> {
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
                    State::HL | State::HLSelected | State::HLClashing => Color {
                        r: base_col.r * 0.7,
                        g: base_col.g * 0.7,
                        b: base_col.b * 0.7,
                        a: 1.0
                    },
                    State::Normal => base_col,
                    State::Selected => base_col.inverse(),
                    State::Clashing => Color::BLACK
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

                frame.stroke(&Path::line(
                        Point { x: bounds.x, y: def },
                        Point { x: bounds.x + bounds.width, y: def }
                ), Stroke::default());
            }
        })]
    }

    fn update(&mut self, event: Event, bounds: Rectangle<f32>, cursor: Cursor) -> (Status, Option<GuiMessage>) {
        use Event::*;
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
