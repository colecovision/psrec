mod gui;
mod view;

use crate::extract::Instance;

#[derive(Clone, Debug)]
pub enum GuiMessage {
    Extracted(Vec<Instance>),
    InstanceHighlighted(usize),
    InstanceSelected(usize),
    Unhighlight,
    Copy
}

pub use gui::*;
