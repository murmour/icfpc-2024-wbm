use std::{collections::HashMap, fs, io::{self, BufRead}, rc::Rc, time::Duration};
use itertools::Itertools;
use lazy_static::lazy_static;
use std::thread;
use std::time;

#[derive(Debug, Clone, Copy)]
enum UnaryOp {
    Neg,
    Not,
    StrToInt,
    IntToStr,
}

type NumType = i64;
type VarType = u64;

impl UnaryOp {
    pub fn parse(body: &[char]) -> UnaryOp  {
        assert!(body.len() == 1);
        match body[0] {
            '-' => UnaryOp::Neg,
            '!' => UnaryOp::Not,
            '#' => UnaryOp::StrToInt,
            '$' => UnaryOp::IntToStr,
            _ => panic!()
        }
    }

    pub fn to_string(&self) -> String {
        let s = match self {
            UnaryOp::Neg => "-",
            UnaryOp::Not => "!",
            UnaryOp::StrToInt => "#",
            UnaryOp::IntToStr => "$",
        };
        String::from(s)
    }
}

#[derive(Debug, Clone, Copy)]
enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Less,
    Gt,
    Eq,
    Or,
    And,
    Concat,
    Take,
    Drop,
    Apply,
}

impl BinaryOp {
    pub fn parse(body: &[char]) -> BinaryOp  {
        assert!(body.len() == 1);
        match body[0] {
            '+' => BinaryOp::Add,
            '-' => BinaryOp::Sub,
            '*' => BinaryOp::Mul,
            '/' => BinaryOp::Div,
            '%' => BinaryOp::Mod,
            '<' => BinaryOp::Less,
            '>' => BinaryOp::Gt,
            '=' => BinaryOp::Eq,
            '|' => BinaryOp::Or,
            '&' => BinaryOp::And,
            '.' => BinaryOp::Concat,
            'T' => BinaryOp::Take,
            'D' => BinaryOp::Drop,
            '$' => BinaryOp::Apply,
            _ => panic!()
        }
    }

    pub fn to_string(&self) -> String {
        let s = match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Mod => "%",
            BinaryOp::Less => "<",
            BinaryOp::Gt => ">",
            BinaryOp::Eq => "=",
            BinaryOp::Or => "|",
            BinaryOp::And => "&",
            BinaryOp::Concat => ".",
            BinaryOp::Take => "T",
            BinaryOp::Drop => "D",
            BinaryOp::Apply => "$",
        };
        String::from(s)
    }

}

#[derive(Debug)]
enum Token {
    True,
    False,
    Int(NumType),
    Str(String),
    Unary(UnaryOp),
    Binary(BinaryOp),
    If,
    Lambda(VarType),
    Var(VarType),
}

fn make_rev_map(m: &Vec<char>) -> HashMap<char, char> {
    let mut res = HashMap::new();
    for (i, c) in m.iter().enumerate() {
        res.insert(*c, (i as u8 + 33) as char);
    }
    res
}

lazy_static! {
    static ref CHAR_MAP: Vec<char> = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n".chars().collect();
    static ref INV_MAP: HashMap<char, char> = make_rev_map(&CHAR_MAP);
}

fn make_string_token(s: &str) -> String {
    let t: String = s.chars().map(|c| INV_MAP.get(&c).unwrap()).collect();
    String::from("S") + &t
}

fn parse_num(data: &[char]) -> NumType {
    let mut res = 0;
    for c in data {
        let dig = *c as u32 - 33;
        res = res * 94 + dig as NumType;
    }
    res
}

fn print_num(mut x: NumType) -> String {
    if x < 0 { panic!() }
    let mut res = vec![];
    while x > 0 {
        let d = x % 94;
        res.push(d);
        x = x / 94;
    }
    if res.is_empty() { res.push(0) }
    res.reverse();
    res.iter().map(|c| CHAR_MAP[*c as usize]).collect()
}

fn parse_var(data: &[char]) -> VarType {
    let mut res = 0;
    for c in data {
        let dig = *c as u32 - 33;
        res = res * 94 + dig as VarType;
    }
    res
}

impl Token {
    pub fn parse(s: &str) -> Result<Token, String> {
        let key = s.chars().next().ok_or("empty token".to_string())?;
        let body: Vec<char> = s.chars().skip(1).collect();
        let res = match key {
            'T' => {
                assert!(body.is_empty());
                Token::True
            },
            'F' => {
                assert!(body.is_empty());
                Token::False
            },
            '?' => {
                assert!(body.is_empty());
                Token::If
            },
            'S' => {
                Token::Str(body.into_iter().map(|c| CHAR_MAP[(c as u32 - 33) as usize]).collect())
            },
            'U' => {
                Token::Unary(UnaryOp::parse(&body))
            },
            'B' => {
                Token::Binary(BinaryOp::parse(&body))
            },
            'L' => {
                Token::Lambda(parse_var(&body))
            },
            'v' => {
                Token::Var(parse_var(&body))
            },
            'I' => {
                Token::Int(parse_num(&body))
            },
            _ => panic!()
        };
        Ok(res)
    }
}

#[derive(Debug)]
enum Expr {
    True,
    False,
    Int(NumType),
    Str(String),
    Unary(UnaryOp, Rc<Expr>),
    Binary(BinaryOp, Rc<Expr>, Rc<Expr>),
    If(Rc<Expr>, Rc<Expr>, Rc<Expr>),
    Lambda(VarType, Rc<Expr>),
    Var(VarType),
}

type PExpr = Rc<Expr>;

impl Expr {
    pub fn to_string(&self) -> String {
        match self {
            Expr::True => String::from("True"),
            Expr::False => String::from("False"),
            Expr::Int(x) => x.to_string(),
            Expr::Str(s) => s.clone(),
            Expr::Unary(op, e) => op.to_string() + &e.to_string(),
            Expr::Binary(op, a, b) => format!("{} {} {}", a.to_string(), op.to_string(), b.to_string()),
            Expr::If(c, a, b) => format!("{} ? {} : {}", c.to_string(), a.to_string(), b.to_string()),
            Expr::Lambda(x, e) => format!("λ{} {}", x, e.to_string()),
            Expr::Var(x) => format!("x{}", x),
        }
    }

    fn int(&self) -> NumType {
        match self {
            Expr::Int(x) => *x,
            _ => panic!()
        }
    }

    fn bool(&self) -> bool {
        match self {
            Expr::True => true,
            Expr::False => false,
            _ => panic!()
        }
    }

    fn str(&self) -> &str {
        match self {
            Expr::Str(s) => s,
            _ => panic!()
        }
    }
}


struct ExprBuilder {
    e_true: PExpr,
    e_false: PExpr,
}

impl ExprBuilder {
    fn new() -> Self {
        Self { e_true: Rc::new(Expr::True), e_false: Rc::new(Expr::False) }
    }

    fn make_expr_rec(&self, tokens: &[Token]) -> (PExpr, usize) {
        match &tokens[0] {
            Token::True => (self.e_true.clone(), 1),
            Token::False => (self.e_false.clone(), 1),
            Token::Int(x) => (Rc::new(Expr::Int(*x)), 1),
            Token::Str(s) => (Rc::new(Expr::Str(s.clone())), 1),
            Token::Unary(op) => {
                let (e, n) = self.make_expr_rec(&tokens[1..]);
                (Rc::new(Expr::Unary(*op, e)), 1 + n)
            },
            Token::Binary(op) => {
                let (e, n) = self.make_expr_rec(&tokens[1..]);
                let (e2, n2) = self.make_expr_rec(&tokens[1 + n ..]);
                (Rc::new(Expr::Binary(*op, e, e2)), 1 + n + n2)
            }
            Token::If => {
                let (e, n) = self.make_expr_rec(&tokens[1..]);
                let (e2, n2) = self.make_expr_rec(&tokens[1 + n ..]);
                let (e3, n3) = self.make_expr_rec(&tokens[1 + n + n2 ..]);
                (Rc::new(Expr::If(e, e2, e3)), 1 + n + n2 + n3)
            },
            Token::Lambda(x) => {
                let (e, n) = self.make_expr_rec(&tokens[1..]);
                (Rc::new(Expr::Lambda(*x, e)), 1 + n)
            },
            Token::Var(x) => (Rc::new(Expr::Var(*x)), 1),
        }
    }

    fn make_expr(&self, tokens: &[Token]) -> Option<PExpr> {
        let (e, n) = self.make_expr_rec(tokens);
        if n == tokens.len() { Some(e) } else { None }
    }

    fn eval_expr(&self, expr: PExpr, env: &mut HashMap<VarType, PExpr>) -> PExpr {
        match expr.as_ref() {
            Expr::True => expr,
            Expr::False => expr,
            Expr::Int(_) => expr,
            Expr::Str(_) => expr,
            Expr::Unary(op, e) => {
                let e = self.eval_expr(e.clone(), env);
                match op {
                    UnaryOp::Not => {
                        match e.as_ref() {
                            Expr::True => self.e_false.clone(),
                            Expr::False => self.e_true.clone(),
                            _ => panic!(),
                        }
                    },
                    UnaryOp::Neg => {
                        match e.as_ref() {
                            Expr::Int(x) => Rc::new(Expr::Int(-x)),
                            _ => panic!(),
                        }
                    },
                    UnaryOp::StrToInt => {
                        match e.as_ref() {
                            Expr::Str(s) => {
                                //let chars: Vec<_> = s.chars().collect();
                                let chars: Vec<_> = s.chars().map(|c| *INV_MAP.get(&c).unwrap()).collect();
                                Rc::new(Expr::Int( parse_num(&chars) ))
                            }
                            _ => panic!(),
                        }
                    },
                    UnaryOp::IntToStr => {
                        match e.as_ref() {
                            Expr::Int(x) => Rc::new(Expr::Str(print_num(*x))),
                            _ => panic!(),
                        }
                    },
                }
            },
            Expr::Binary(op, a, b) => {
                if matches!(op, BinaryOp::Apply) {
                    let val = self.eval_expr(b.clone(), env);
                    let mut cur = a.clone();
                    loop {
                        match cur.as_ref() {
                            Expr::Lambda(var, inner) => {
                                env.insert(*var, val);
                                return self.eval_expr(inner.clone(), env)
                            },
                            _ => ()
                        }
                        cur = self.eval_expr(cur, env);
                    }
                }
                let a = self.eval_expr(a.clone(), env);
                let b = self.eval_expr(b.clone(), env);
                match op {
                    BinaryOp::Add => Rc::new(Expr::Int(a.int() + b.int())),
                    BinaryOp::Sub => Rc::new(Expr::Int(a.int() - b.int())),
                    BinaryOp::Mul => Rc::new(Expr::Int(a.int() * b.int())),
                    BinaryOp::Div => Rc::new(Expr::Int(a.int() / b.int())),
                    BinaryOp::Mod => Rc::new(Expr::Int(a.int() % b.int())),
                    BinaryOp::Less => if a.int() < b.int() { self.e_true.clone() } else { self.e_false.clone() },
                    BinaryOp::Gt => if a.int() > b.int() { self.e_true.clone() } else { self.e_false.clone() },
                    BinaryOp::Eq => {
                        let res = match (a.as_ref(), b.as_ref()) {
                            (Expr::Int(x), Expr::Int(y)) => x == y,
                            (Expr::Str(x), Expr::Str(y)) => x == y,
                            (Expr::True, Expr::True) => true,
                            (Expr::False, Expr::False) => true,
                            (Expr::True, Expr::False) => false,
                            (Expr::False, Expr::True) => false,
                            _ => panic!()
                        };
                        if res { self.e_true.clone() } else { self.e_false.clone() }
                    },
                    BinaryOp::Or => if a.bool() || b.bool() { self.e_true.clone() } else { self.e_false.clone() },
                    BinaryOp::And => if a.bool() && b.bool() { self.e_true.clone() } else { self.e_false.clone() },
                    BinaryOp::Concat => Rc::new(Expr::Str(String::from(a.str()) + b.str())),
                    BinaryOp::Take => Rc::new(Expr::Str(b.str().chars().take(a.int() as usize).collect())),
                    BinaryOp::Drop => Rc::new(Expr::Str(b.str().chars().skip(a.int() as usize).collect())),
                    BinaryOp::Apply => panic!(),
                }
            },
            Expr::If(a, b, c) => {
                let a = self.eval_expr(a.clone(), env);
                if a.bool() {
                    self.eval_expr(b.clone(), env)
                } else {
                    self.eval_expr(c.clone(), env)
                }
            },
            Expr::Lambda(_, _) => expr,
            Expr::Var(x) => {
                match env.get(x) {
                    Some(v) => v.clone(),
                    None => expr,
                }
            },
        }
    }

    fn eval_string(&self, s: &str) -> PExpr {
        let tokens = s.split_ascii_whitespace().map(|s| Token::parse(s).unwrap()).collect_vec();
        let e = self.make_expr(&tokens).unwrap();
        //println!("{:?}", e);
        let mut env = HashMap::new();
        let e = self.eval_expr(e, &mut env);
        e
    }

}


fn repl() {
    let stdin = io::stdin();
    let eb = ExprBuilder::new();
    for line in stdin.lock().lines() {
        let s = line.unwrap();
        let e = eb.eval_string(&s);
        println!("{:?}", e);
    }
}

fn communicate() {
    //let client = reqwest::blocking::Client::new();
    //let body = "S'%4}).$%8";
    //let body = make_string_token("get language_test");
    //let body = make_string_token("solve language_test 4w3s0m3");
    //let body = make_string_token("get lambdaman");
    //let body = make_string_token("get lambdaman1");
    let body = make_string_token("get 3d");
    println!("{}", body);
    //return;
    //let expr = "S'%4}).$%8";
    let client = reqwest::blocking::Client::new();
    let expr = client.post("https://boundvariable.space/communicate")
        .body(body)
        .bearer_auth("00814e29-be7a-4b5a-ad13-ee45f0c5c37c")
        .send().unwrap().text().unwrap();
    //let tokens = expr.split_ascii_whitespace().map(|s| Token::parse(s).unwrap()).collect_vec();
    //println!("{:?}", tokens);
    println!("{}", expr);
    let eb = ExprBuilder::new();
    let e = eb.eval_string(&expr);
    println!("{:?}", e);
    match e.as_ref() {
        Expr::Str(s) => {
            println!("{}", s);
        },
        _ => ()
    }
}

fn communicate_msg(msg: &str) -> String {
    let client = reqwest::blocking::Client::new();
    let msg = make_string_token(msg);
    let expr = client.post("https://boundvariable.space/communicate")
        .body(msg)
        .bearer_auth("00814e29-be7a-4b5a-ad13-ee45f0c5c37c")
        .send().unwrap().text().unwrap();

    println!("{}", expr);
    let eb = ExprBuilder::new();
    let e = eb.eval_string(&expr);
    //println!("{:?}", e);
    match e.as_ref() {
        Expr::Str(s) => {
            s.clone()
        },
        _ => e.to_string()
    }
}

fn download_lambdaman() {
    for i in 1..=21 {
        let t = communicate_msg(&format!("get lambdaman{}", i));
        let fname = format!("../../data/lambdaman/{:02}.in", i);
        fs::write(&fname, t).unwrap();
        println!("Saved {}", fname);
        thread::sleep(Duration::from_secs(5));
    }
}

fn main_inner() {
    //repl()
    //communicate()
    download_lambdaman()
}

const STACK_SIZE: usize = 128 * 1024 * 1024;

fn main() {
    // Spawn thread with explicit stack size
    let child = thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(main_inner)
        .unwrap();

    // Wait for thread to join
    child.join().unwrap();
}
