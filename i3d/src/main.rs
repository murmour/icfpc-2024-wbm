
use std::collections::HashMap;

use rug::{Complete, Integer};
use table_enum::table_enum;

table_enum! {
    #[derive(Clone, Debug, PartialEq)]
    enum Operator(#[constructor] symbol: char) {
        Left('<'),
        Right('>'),
        Up('^'),
        Down('v'),
        Add('+'),
        Sub('-'),
        Mul('*'),
        Div('/'),
        Mod('%'),
        Warp('@'),
        Eq('='),
        Neq('#'),
        Submit('S'),
        InputA('A'),
        InputB('B'),
    }
}

#[derive(Clone, Debug, PartialEq)]
enum Cell {
    Empty,
    Consumed, // special value used during computation
    Num(Integer),
    Op(Operator),
}

impl Cell {
    fn is_value(&self) -> bool {
        !matches!(&self, Cell::Empty | Cell::Consumed)
    }
    fn num(&self) -> Option<&Integer> {
        if let Cell::Num(x) = self {
            Some(x)
        } else {
            None
        }
    }
}

#[derive(Debug)]
struct State {
    cells: Vec<Vec<Cell>>,
    t: usize,
}

enum UpdateResult {
    Ok(State),
    Warp(usize, HashMap<(i32, i32), Cell>), // t, (x, y) -> Value
    Stop(Cell),
}

type Arr = Vec<Vec<Cell>>;

struct Warp {
    x: i32,
    y: i32,
    dt: usize,
    v: Cell,
}

impl State {
    fn update(&self) -> UpdateResult {
        let n = self.cells.len();
        let m = self.cells[0].len();
        let mut res = vec![vec![Cell::Empty; m]; n];

        let consume = |a: &mut Arr, i: usize, j: usize| {
            if matches!(a[i][j], Cell::Empty) {
                a[i][j] = Cell::Consumed;
            }
        };

        let write = |a: &mut Arr, i: usize, j: usize, c: Cell| {
            if matches!(a[i][j], Cell::Empty | Cell::Consumed) {
                a[i][j] = c;
            } else {
                panic!("conflicting write!");
            }
        };

        let mut warps = vec![];
        let mut reduced = false;

        for i in 1..n-1 {
            for j in 1..m-1 {

                let left = || &self.cells[i][j-1];
                let right =  || &self.cells[i][j+1];
                let up = || &self.cells[i-1][j];
                let down = || &self.cells[i+1][j];

                let binary = |a: &mut Arr, f: fn(&Integer, &Integer) -> Integer, allow_zero: bool| {
                    if let (Some(x), Some(y)) = (left().num(), up().num()) {
                        if !allow_zero && *y == Integer::ZERO { return false }
                        consume(a, i-1, j);
                        consume(a, i, j-1);
                        let z = f(x, y);
                        write(a, i+1, j, Cell::Num(z.clone()));
                        write(a, i, j+1, Cell::Num(z));
                        true
                    } else {
                        false
                    }
                };

                match &self.cells[i][j] {
                    Cell::Empty  => (),
                    Cell::Num(_) => (),
                    Cell::Op(op) => match op {
                        Operator::Right => {
                            if left().is_value() {
                                consume(&mut res, i, j-1);
                                write(&mut res, i, j+1, left().clone());
                                reduced = true;
                            }
                        },
                        Operator::Left => {
                            if right().is_value() {
                                consume(&mut res, i, j+1);
                                write(&mut res, i, j-1, right().clone());
                                reduced = true;
                            }
                        },
                        Operator::Up => {
                            if down().is_value() {
                                consume(&mut res, i+1, j);
                                write(&mut res, i-1, j, down().clone());
                                reduced = true;
                            }
                        },
                        Operator::Down => {
                            if up().is_value() {
                                consume(&mut res, i-1, j);
                                write(&mut res, i+1, j, up().clone());
                                reduced = true;
                            }
                        },
                        Operator::Add => {
                            if binary(&mut res, |a, b| (a + b).complete(), true) {
                                reduced = true;
                            }
                        }
                        Operator::Sub => {
                            if binary(&mut res, |a, b| (a - b).complete(), true) {
                                reduced = true;
                            }
                        },
                        Operator::Mul => {
                            if binary(&mut res, |a, b| (a * b).complete(), true) {
                                reduced = true;
                            }
                        },
                        Operator::Div => {
                            if binary(&mut res, |a, b| (a / b).complete(), false) {
                                reduced = true;
                            }
                        },
                        Operator::Mod => {
                            if binary(&mut res, |a, b| (a % b).complete(), false) {
                                reduced = true;
                            }
                        }
                        Operator::Warp => {
                            if up().is_value() {
                                if let (Some(dx), Some(dy), Some(dt)) = (left().num(), right().num(), down().num()) {
                                    if *dt > 0 && *dt < self.t {
                                        warps.push(Warp { x: i as i32 - dy.to_i32().unwrap(), y: j as i32 - dx.to_i32().unwrap(), dt: dt.to_usize().unwrap(), v: up().clone()});
                                    }
                                }
                            }
                        },
                        Operator::Eq => {
                            if left().is_value() && left() == up() {
                                consume(&mut res, i-1, j);
                                consume(&mut res, i, j-1);
                                let z = left().clone();
                                write(&mut res, i+1, j, z.clone());
                                write(&mut res, i, j+1, z);
                                reduced = true;
                            }
                        },
                        Operator::Neq => {
                            if left().is_value() && left() != up() {
                                consume(&mut res, i-1, j);
                                consume(&mut res, i, j-1);
                                write(&mut res, i+1, j, left().clone());
                                write(&mut res, i, j+1, up().clone());
                                reduced = true;
                            }
                        },
                        Operator::Submit => (),
                        Operator::InputA => panic!(),
                        Operator::InputB => panic!(),
                    },
                    Cell::Consumed => panic!(),
                }
            }
        }
        // check if submit is overwritten
        let mut submit = None;
        for i in 0 .. n {
            for j in 0 .. m {
                if matches!(self.cells[i][j], Cell::Op(Operator::Submit)) && res[i][j].is_value() {
                    assert!(submit.is_none(), "multiple submit");
                    submit = Some(res[i][j].clone())
                }
            }
        }
        if let Some(x) = submit {
            UpdateResult::Stop(x)
        } else if !warps.is_empty() {
            let dt = warps[0].dt;
            assert!(warps.iter().all(|w| w.dt == dt), "different dt!");
            let mut wmap = HashMap::new();
            for Warp { x, y, dt: _, v } in warps {
                if let Some(old) = wmap.insert((x, y), v.clone()) {
                    assert!(old == v, "wrap writes different values");
                }
            }
            UpdateResult::Warp(self.t - dt, wmap)
        } else if !reduced {
            UpdateResult::Stop(Cell::Empty)
        } else {
            for i in 0 .. n {
                for j in 0 ..m {
                    if matches!(res[i][j], Cell::Consumed) {
                        res[i][j] = Cell::Empty
                    } else if matches!(res[i][j], Cell::Empty) {
                        res[i][j] = self.cells[i][j].clone()
                    }
                }
            }
            UpdateResult::Ok(State { cells: res, t: self.t + 1 })
        }
    }

    fn from_string(s: &str, a: Integer, b: Integer) -> State {
        let lines: Vec<_> = s.lines().collect();
        let h = lines.len();
        let w = lines.iter().map(|s| s.split_ascii_whitespace().count()).max().unwrap();
        let mut cells = vec![vec![Cell::Empty; w + 2]; h + 2];
        for i in 0 .. h {
            for (j, s) in lines[i].split_ascii_whitespace().enumerate() {
                if s == "." { continue; }
                cells[i+1][j+1] = if let Some(op) = Operator::new(s.chars().nth(0).unwrap()) {
                    match op {
                        Operator::InputA => Cell::Num(a.clone()),
                        Operator::InputB => Cell::Num(b.clone()),
                        _ => Cell::Op(op)
                    }
                } else {
                    let n: i32 = s.parse().unwrap();
                    assert!(n >= -99 && n <= 99);
                    Cell::Num(n.into())
                }
            }
        }
        State { cells, t: 1 }
    }
}

fn simulate(s0: State) -> Cell {
    let mut states = vec![s0];
    loop {
        match states.last().unwrap().update() {
            UpdateResult::Ok(s) => states.push(s),
            UpdateResult::Warp(t, vals) => {
                //println!("warping to {} : {:?}", t, vals);
                while states.len() > t { states.pop(); }
                for ((x, y), v) in vals.into_iter() {
                    states[t-1].cells[x as usize][y as usize] = v;
                }
            },
            UpdateResult::Stop(res) => {
                return res;
            },
        }
    }
}

fn test() {
    let summ = "#
    . . . . 0 . . . .
    . B > . = . . . .
    . v 1 . . > . . .
    . . - . . . + S .
    . . . . . ^ . . .
    . . v . . 0 > . .
    . . . . . . A + .
    . 1 @ 6 . . < . .
    . . 3 . 0 @ 3 . .
    . . . . . 3 . . .
    #";
    let s0 = State::from_string(summ, 7.into(), 8.into());
    println!("{:?}", s0);
    let res = simulate(s0);
    println!("{:?}", res);
}

fn main() {
    test();
}
