mod interpreter;

// fn test() {
//     let summ = "#
//     . . . . 0 . . . .
//     . B > . = . . . .
//     . v 1 . . > . . .
//     . . - . . . + S .
//     . . . . . ^ . . .
//     . . v . . 0 > . .
//     . . . . . . A + .
//     . 1 @ 6 . . < . .
//     . . 3 . 0 @ 3 . .
//     . . . . . 3 . . .
//     #";
//     let s0 = State::from_string(summ, 7.into(), 8.into());
//     println!("{:?}", s0);
//     let res = simulate(s0);
//     println!("{:?}", res);
// }

// fn main() {
//     test();
// }

use std::{cell::RefCell, fs, rc::Rc, str::FromStr};

use enums::{Key, Shortcut};
use fltk::{prelude::*, *};
use interpreter::{Cell, Operator, State, UpdateResult};
use anyhow::Result as R;
use itertools::Itertools;
use menu::MenuFlag;
use rug::Integer;

enum SimulationStatus {
    Initial,
    InProgress,
    Finished(Cell),
}

struct Simulation {
    states: Vec<State>,
    status: SimulationStatus,
    tick: usize,
}

impl Simulation {
    fn new(s0: State) -> Self {
        Self { states: vec![s0], status: SimulationStatus::Initial, tick: 1 }
    }

    fn cur_state(&self) -> &State {
        self.states.last().unwrap()
    }

    fn set_input(&mut self, a: &str, b: &str) -> R<()> {
        if matches!(self.status, SimulationStatus::Initial) {
            let a = Integer::from_str(a)?;
            let b = Integer::from_str(b)?;
            self.states[0].instantiate(a, b);
            self.status = SimulationStatus::InProgress;
        }
        Ok(())
    }

    fn step(&mut self) -> R<()> {
        if matches!(self.status, SimulationStatus::InProgress) {
            self.tick += 1;
            match self.states.last().unwrap().update()? {
                UpdateResult::Ok(s) => self.states.push(s),
                UpdateResult::Warp(t, vals) => {
                    //println!("warping to {} : {:?}", t, vals);
                    while self.states.len() > t { self.states.pop(); }
                    for ((x, y), v) in vals.into_iter() {
                        self.states[t-1].cells[x as usize][y as usize] = v;
                    }
                },
                UpdateResult::Stop(res) => {
                    self.status = SimulationStatus::Finished(res)
                },
            }
        }
        Ok(())
    }
}

fn event_char() -> Option<char> {
    if app::event_key().bits() > 128 { return None; }
    if app::event_key_down(Key::ShiftL) || app::event_key_down(Key::ShiftR) {
        let t = app::event_key().to_char()?;
        match t {
            'a'..='z' => Some(t.to_ascii_uppercase()),
            '0' => Some(')'),
            '1' => Some('!'),
            '2' => Some('@'),
            '3' => Some('#'),
            '4' => Some('$'),
            '5' => Some('%'),
            '6' => Some('^'),
            '7' => Some('&'),
            '8' => Some('*'),
            '9' => Some('('),
            '/' => Some('?'),
            '-' => Some('_'),
            '=' => Some('+'),
            '`' => Some('~'),
            ';' => Some(':'),
            '.' => Some('>'),
            ',' => Some('<'),
            _ => None
        }
    } else {
        app::event_key().to_char()
    }
}

fn main() {
    const CELL_SIZE: i32 = 30;
    let summ = "
    . . . . 0 . . . .
    . B > . = . . . .
    . v 1 . . > . . .
    . . - . . . + S .
    . . . . . ^ . . .
    . . v . . 0 > . .
    . . . . . . A + .
    . 1 @ 6 . . < . .
    . . 3 . 0 @ 3 . .
    . . . . . 3 . . .";
    let s0 = Rc::new(RefCell::new(State::from_string(summ)));
    let sim0 = Rc::new(RefCell::new(Simulation::new(s0.borrow().clone())));

    let app = app::App::default().with_scheme(app::Scheme::Gtk);
    let mut wind = window::Window::default().with_size(800, 600);
    let mut layout = group::Flex::default_fill().column();
    let toolbar = group::Pack::default_fill().with_size(0, 25).with_type(group::PackType::Horizontal);
    let mut button_step = button::Button::default().with_size(100, 0).with_label("Step");
    let mut button_reset = button::Button::default().with_size(100, 0).with_label("Reset");
    let mut button_load = button::Button::default().with_size(100, 0).with_label("Load");
    let mut button_save = button::Button::default().with_size(100, 0).with_label("Save");
    let _ = frame::Frame::default().with_label("A:").with_size(20, 0);
    let mut input_a = input::Input::default().with_size(150, 0);
    input_a.set_value("5");
    let _ = frame::Frame::default().with_label("B:").with_size(20, 0);
    let mut input_b = input::Input::default().with_size(150, 0);
    input_b.set_value("7");
    toolbar.end();
    layout.fixed(&toolbar, 25);

    let mut table = table::Table::default();
    table.set_rows(sim0.borrow().cur_state().cells.len() as i32 - 2);
    //table.set_row_header(true);
    table.set_row_resize(true);
    table.set_row_height_all(CELL_SIZE);
    table.set_cols(sim0.borrow().cur_state().cells[0].len() as i32 - 2);
    //table.set_col_header(true);
    table.set_col_width_all(CELL_SIZE);
    table.set_col_resize(true);
    table.end();

    let status0 = frame::Frame::default().with_size(200, 20).with_label("Ready");
    layout.fixed(&status0, 20);
    layout.end();

    wind.resizable(&layout);
    wind.end();
    wind.show();


    let sim = sim0.clone();
    let mut status = status0.clone();
    let mut table_ = table.clone();
    let s0_ = s0.clone();
    let mut reset = move || {
        sim.replace(Simulation::new(s0_.borrow().clone()));
        status.set_label("Ready");
        table_.set_rows(sim.borrow().cur_state().cells.len() as i32 - 2);
        table_.set_cols(sim.borrow().cur_state().cells[0].len() as i32 - 2);
        table_.redraw();
    };

    let s0_ = s0.clone();
    let mut reset_ = reset.clone();
    button_load.set_callback(move |_| {
        let mut dialog = dialog::NativeFileChooser::new(dialog::NativeFileChooserType::BrowseFile);
        dialog.show();
        if !dialog.filename().exists() { return; }
        let data = fs::read_to_string(dialog.filename()).unwrap();
        s0_.replace(State::from_string(&data));
        reset_();
    });

    let s0_ = s0.clone();
    button_save.set_callback(move |_| {
        let mut dialog = dialog::NativeFileChooser::new(dialog::NativeFileChooserType::BrowseSaveFile);
        dialog.show();
        let data = s0_.borrow().to_string();
        if fs::write(dialog.filename(), data).is_err() {
            dialog::alert_default("Save failed");
        }
    });

    let sim = sim0.clone();
    let mut status = status0.clone();
    let mut table_ = table.clone();
    let s0_ = s0.clone();
    button_step.set_callback(move |_| {
        if matches!(sim.borrow().status, SimulationStatus::Initial) {
            sim.replace(Simulation::new(s0_.borrow().clone())); // apply changes
        }
        if let Err(e) = sim.borrow_mut().set_input(&input_a.value(), &input_b.value()) {
            status.set_label(&e.to_string());
            return;
        }
        if let Err(e) = sim.borrow_mut().step() {
            status.set_label(&e.to_string());
            return;
        }
        if let SimulationStatus::Finished(c) = &sim.borrow().status {
            status.set_label(&format!("Computation finished: {}", c.to_string()))
        } else {
            status.set_label(&format!("Tick = {}, t = {}", sim.borrow().tick, sim.borrow().states.len()))
        }
        table_.redraw();
    });


    let mut reset_ = reset.clone();
    button_reset.set_callback(move |_| {
        reset_();
    });

    // Called when the table is drawn then when it's redrawn due to events
    let s0_ = s0.clone();
    let sim = sim0.clone();
    table.draw_cell(move |t, ctx, row, col, x, y, w, h| match ctx {
        table::TableContext::StartPage => draw::set_font(enums::Font::Helvetica, 14),
        table::TableContext::Cell => {
            let ri = row as usize + 1;
            let ci = col as usize + 1;
            let c = if matches!(sim.borrow().status, SimulationStatus::Initial) {
                s0_.borrow().cells[ri][ci].clone()
            } else {
                sim.borrow().cur_state().cells[ri][ci].clone()
            };
            let col =
            if t.is_selected(row, col) { enums::Color::from_u32(0x00D3_D3D3) } else {
                match &c {
                    Cell::Op(op) => {
                        if let Operator::Submit = op { enums::Color::from_rgb(255, 192, 192) } else { enums::Color::from_rgb(192, 255, 192) }
                    }
                    _ => enums::Color::White
                }
            };
            let mut s = c.to_string();
            if s == "." { s = String::new() }
            draw_data(
                &s,
                x,
                y,
                w,
                h,
                col,
            );
            // if let Cell::Op(Operator::Warp) = &c {
            //     let dx = if let Cell::Num(x) = &s0_.borrow().cells[ri][ci-1] { Some(x.to_i32_wrapping()) } else { None };
            //     let dy = if let Cell::Num(x) = &s0_.borrow().cells[ri][ci+1] { Some(x.to_i32_wrapping()) } else { None };
            //     if let (Some(dx), Some(dy)) = (dx, dy) {
            //         draw::set_color_rgb(0, 0, 255);
            //         draw::draw_line(x + 20, y + 20, x - CELL_SIZE * dx + 20, y - CELL_SIZE * dy + 20);
            //     }
            // }
        }, // Data in cells
        _ => (),
    });

    let clipboard = Rc::new(RefCell::new(None));

    let s0_ = s0.clone();
    table.handle(move|t, e| {
        match e {
            enums::Event::Push => {
                t.take_focus().unwrap();
                if app::event_mouse_button() == app::MouseButton::Right {
                    if !matches!(sim0.borrow().status, SimulationStatus::Initial) { return false; }
                    if let Some((r1, c1, r2, c2)) = t.try_get_selection() {
                        let (ex, ey) = app::event_coords();
                        let menu = menu::MenuItem::new(&["Copy", "Cut", "Paste"]);
                        if let Some(m) = menu.popup(ex, ey) {
                            if let Some(label) = m.label() {
                                if label == "Copy" || label == "Cut" {
                                    let data = s0_.borrow().cells[r1 as usize + 1 ..= r2 as usize + 1].iter().
                                        map(|row| { row[c1 as usize + 1 ..= c2 as usize + 1].iter().cloned().collect_vec() }).collect_vec();
                                    clipboard.replace(Some(data));
                                    if label == "Cut" {
                                        for i in r1 as usize ..= r2 as usize {
                                            for j in c1 as usize ..= c2 as usize {
                                                s0_.borrow_mut().cells[i+1][j+1] = Cell::Empty;
                                            }
                                        }
                                        reset();
                                    }
                                } else { // Paste
                                    if let Some(data) = clipboard.borrow().as_ref() {
                                        {
                                            let mut s0_b = s0_.borrow_mut();
                                            let n_old = s0_b.cells.len();
                                            let m_old = s0_b.cells[0].len();
                                            let n_new = r1 as usize + 1 + data.len() + 1;
                                            let m_new = c1 as usize + 1 + data[0].len() + 1;
                                            if n_new > n_old { s0_b.cells.resize(n_new, vec![Cell::Empty; m_new]) }
                                            if m_new > m_old { for row in s0_b.cells.iter_mut() { row.resize(m_new, Cell::Empty) } }
                                            for (i, row) in data.iter().enumerate() {
                                                for (j, c) in row.iter().enumerate() {
                                                    s0_b.cells[r1 as usize + 1 + i][c1 as usize + 1 + j] = c.clone();
                                                }
                                            }
                                        }
                                        reset();
                                    }
                                }
                            }
                        }
                    }
                }
            },
            enums::Event::KeyDown => {
                if !matches!(sim0.borrow().status, SimulationStatus::Initial) { return false; }
                //println!("{:?}", event_char());
                if app::event_key() == enums::Key::Delete {
                    if let Some((r1, c1, r2, c2)) = t.try_get_selection() {
                        for r in r1..=r2 {
                            for c in c1..=c2 {
                                s0_.borrow_mut().cells[r as usize + 1][c as usize + 1] = Cell::Empty;
                            }
                        }
                    }
                    t.redraw();
                    return false;
                }
                let mut cell = None;
                if event_char() == Some('.') {
                    cell = Some(Cell::Empty);
                } else if let Some(op) = Operator::new(event_char().unwrap_or('x')) {
                    cell = Some(Cell::Op(op))
                } else if matches!(event_char(), Some('0'..='9')) {
                    cell = Some(Cell::Num((event_char().unwrap() as i32 - '0' as i32).into()))
                } else if event_char() == Some('n') {
                    if let Some(s) = dialog::input_default("Enter number:", "") {
                        if let Ok(x) = s.parse::<i32>() {
                            if -100 < x && x < 100 {
                                cell = Some(Cell::Num(x.into()))
                            }
                        }
                    }
                }
                if let Some(cell) = cell {
                    if let Some((r, c, _, _)) = t.try_get_selection() {
                        s0_.borrow_mut().cells[r as usize + 1][c as usize + 1] = cell;
                        t.redraw();
                    }
                }
            },
            _ => ()
        }
        false
    });

    app.run().unwrap();
}

fn draw_data(txt: &str, x: i32, y: i32, w: i32, h: i32, col: enums::Color) {
    draw::push_clip(x, y, w, h);
    draw::set_draw_color(col);
    draw::draw_rectf(x, y, w, h);
    draw::set_draw_color(enums::Color::Gray0);
    draw::set_font(enums::Font::Helvetica, 14);
    draw::draw_text2(txt, x, y, w, h, enums::Align::Center);
    draw::set_draw_rgb_color(192, 192, 192);
    draw::draw_rect(x, y, w, h);
    draw::pop_clip();
}
