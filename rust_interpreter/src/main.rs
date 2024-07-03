use std::{collections::HashMap, fs, io::{self, BufRead}, rc::Rc, time::Duration};
use immutable_map::TreeMap;
use itertools::Itertools;
use lazy_static::lazy_static;
use once_cell::unsync::OnceCell;
use std::thread;
use std::time;
use rug::Integer;
use table_enum::table_enum;

table_enum! {
    #[derive(Debug, Clone, Copy)]
    enum UnaryOp(#[constructor] symbol: char) {
        Neg('-'),
        Not('!'),
        StrToInt('#'),
        IntToStr('$'),
    }
}

type NumType = Integer;
type VarType = u64;

impl UnaryOp {
    pub fn parse(body: &[char]) -> UnaryOp  {
        assert!(body.len() == 1);
        UnaryOp::new(body[0]).unwrap()
    }

    pub fn to_string(&self) -> String {
        self.symbol().to_string()
    }
}

#[derive(Debug)]
struct LazyEval {
    expr: PExpr,
    env: Env,
    res: OnceCell<ER>,
}

impl LazyEval {
    pub fn eval(&self) -> ER {
        self.res.get_or_init(|| eval_expr(self.expr.clone(), self.env.clone())).clone()
    }

    pub fn new(expr: PExpr, env: Env) -> Self {
        LazyEval { expr, env, res: OnceCell::new() }
    }
}

#[derive(Debug)]
enum EvalResult {
    Bool(bool),
    Int(NumType),
    Str(String),
    Lambda(VarType, PExpr, TreeMap<VarType, Rc<LazyEval>>),
}

type ER = Rc<EvalResult>;

impl EvalResult {
    fn int(&self) -> NumType {
        match self {
            EvalResult::Int(x) => x.clone(),
            _ => panic!()
        }
    }

    fn bool(&self) -> bool {
        match self {
            EvalResult::Bool(x) => *x,
            _ => panic!()
        }
    }

    fn str(&self) -> &str {
        match self {
            EvalResult::Str(s) => s,
            _ => panic!()
        }
    }
}

table_enum! {
    #[derive(Debug, Clone, Copy)]
    enum BinaryOp(#[constructor] symbol: char) {
        Add('+'),
        Sub('-'),
        Mul('*'),
        Div('/'),
        Mod('%'),
        Less('<'),
        Gt('>'),
        Eq('='),
        Or('|'),
        And('&'),
        Concat('.'),
        Take('T'),
        Drop('D'),
        Apply('$'),
    }
}

impl BinaryOp {
    pub fn parse(body: &[char]) -> BinaryOp  {
        assert!(body.len() == 1);
        BinaryOp::new(body[0]).unwrap()
    }

    pub fn to_string(&self) -> String {
        self.symbol().to_string()
    }
}

#[derive(Debug)]
enum Token {
    Bool(bool),
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
    let mut res = NumType::ZERO;
    for c in data {
        let dig = *c as u32 - 33;
        res = res * 94 + NumType::from(dig);
    }
    res
}

fn print_num(mut x: NumType) -> String {
    if x < 0 { panic!() }
    let mut res = vec![];
    let base = NumType::from(94);
    while x > 0 {
        let d = (x.clone() % base.clone()).to_i32().unwrap();
        res.push(d);
        x = x / base.clone();
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
                Token::Bool(true)
            },
            'F' => {
                assert!(body.is_empty());
                Token::Bool(false)
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
    Bool(bool),
    Int(NumType),
    Str(String),
    Unary(UnaryOp, PExpr),
    Binary(BinaryOp, PExpr, PExpr),
    If(PExpr, PExpr, PExpr),
    Lambda(VarType, PExpr),
    Var(VarType),
}

type PExpr = Rc<Expr>;

impl Expr {
    pub fn to_string(&self) -> String {
        match self {
            Expr::Bool(b) => b.to_string(),
            Expr::Int(x) => x.to_string(),
            Expr::Str(s) => s.clone(),
            Expr::Unary(op, e) => op.to_string() + &e.to_string(),
            Expr::Binary(op, a, b) => format!("B{} {} {}", op.to_string(), a.to_string(), b.to_string()),
            Expr::If(c, a, b) => format!("({} ? {} : {})", c.to_string(), a.to_string(), b.to_string()),
            Expr::Lambda(x, e) => format!("λ{} {}", x, e.to_string()),
            Expr::Var(x) => format!("x{}", x),
        }
    }
}

type Env = TreeMap<VarType, Rc<LazyEval>>;


fn make_expr_rec(tokens: &[Token]) -> (PExpr, usize) {
    match &tokens[0] {
        Token::Bool(x) => (Rc::new(Expr::Bool(*x)), 1),
        Token::Int(x) => (Rc::new(Expr::Int(x.clone())), 1),
        Token::Str(s) => (Rc::new(Expr::Str(s.clone())), 1),
        Token::Unary(op) => {
            let (e, n) = make_expr_rec(&tokens[1..]);
            (Rc::new(Expr::Unary(*op, e)), 1 + n)
        },
        Token::Binary(op) => {
            let (e, n) = make_expr_rec(&tokens[1..]);
            let (e2, n2) = make_expr_rec(&tokens[1 + n ..]);
            (Rc::new(Expr::Binary(*op, e, e2)), 1 + n + n2)
        }
        Token::If => {
            let (e, n) = make_expr_rec(&tokens[1..]);
            let (e2, n2) = make_expr_rec(&tokens[1 + n ..]);
            let (e3, n3) = make_expr_rec(&tokens[1 + n + n2 ..]);
            (Rc::new(Expr::If(e, e2, e3)), 1 + n + n2 + n3)
        },
        Token::Lambda(x) => {
            let (e, n) = make_expr_rec(&tokens[1..]);
            (Rc::new(Expr::Lambda(*x, e)), 1 + n)
        },
        Token::Var(x) => (Rc::new(Expr::Var(*x)), 1),
    }
}

fn make_expr(tokens: &[Token]) -> Option<PExpr> {
    let (e, n) = make_expr_rec(tokens);
    if n == tokens.len() { Some(e) } else { None }
}

fn er_bool(x: bool) -> ER {
    Rc::new(EvalResult::Bool(x))
}

fn er_int(x: NumType) -> ER {
    Rc::new(EvalResult::Int(x))
}

fn er_str(s: String) -> ER {
    Rc::new(EvalResult::Str(s))
}

fn eval_expr(expr: PExpr, env: Env) -> ER {
    match expr.as_ref() {
        Expr::Bool(x) => er_bool(*x),
        Expr::Int(x) => er_int(x.clone()),
        Expr::Str(s) => er_str(s.clone()),
        Expr::Unary(op, e) => {
            let e = eval_expr(e.clone(), env);
            match op {
                UnaryOp::Not => {
                    er_bool(!e.bool())
                },
                UnaryOp::Neg => {
                    er_int(NumType::ZERO - e.int())
                },
                UnaryOp::StrToInt => {
                    let chars: Vec<_> = e.str().chars().map(|c| *INV_MAP.get(&c).unwrap()).collect();
                    er_int( parse_num(&chars) )
                },
                UnaryOp::IntToStr => {
                    er_str(print_num(e.int()))
                },
            }
        },
        Expr::Binary(op, a, b) => {
            if matches!(op, BinaryOp::Apply) {
                match eval_expr(a.clone(), env.clone()).as_ref() {
                    EvalResult::Lambda(var, body, lam_env) => {
                        let body_env = lam_env.insert(
                            *var,
                            Rc::new(LazyEval::new(b.clone(), env)));
                        return eval_expr(body.clone(), body_env);
                    },
                    _ => panic!()
                }
            }
            let a = eval_expr(a.clone(), env.clone());
            let b = eval_expr(b.clone(), env);
            match op {
                BinaryOp::Add => er_int(a.int() + b.int()),
                BinaryOp::Sub => er_int(a.int() - b.int()),
                BinaryOp::Mul => er_int(a.int() * b.int()),
                BinaryOp::Div => er_int(a.int() / b.int()),
                BinaryOp::Mod => er_int(a.int() % b.int()),
                BinaryOp::Less => er_bool(a.int() < b.int()),
                BinaryOp::Gt => er_bool(a.int() > b.int()),
                BinaryOp::Eq => {
                    let res = match (a.as_ref(), b.as_ref()) {
                        (EvalResult::Int(x), EvalResult::Int(y)) => x == y,
                        (EvalResult::Str(x), EvalResult::Str(y)) => x == y,
                        (EvalResult::Bool(x), EvalResult::Bool(y)) => x == y,
                        _ => panic!()
                    };
                    er_bool(res)
                },
                // todo: short-circuiting
                BinaryOp::Or => er_bool(a.bool() || b.bool()),
                BinaryOp::And => er_bool(a.bool() && b.bool()),
                BinaryOp::Concat => er_str(String::from(a.str()) + b.str()),
                BinaryOp::Take => er_str(b.str().chars().take(a.int().to_usize().unwrap() as usize).collect()),
                BinaryOp::Drop => er_str(b.str().chars().skip(a.int().to_usize().unwrap() as usize).collect()),
                BinaryOp::Apply => panic!(),
            }
        },
        Expr::If(a, b, c) => {
            let a = eval_expr(a.clone(), env.clone());
            if a.bool() {
                eval_expr(b.clone(), env.clone())
            } else {
                eval_expr(c.clone(), env)
            }
        },
        Expr::Lambda(var, body) => {
            Rc::new(EvalResult::Lambda(*var, body.clone(), env))
        }
        Expr::Var(x) => {
            match env.get(x) {
                Some(v) => v.eval(),
                None => {
                    println!("Unbound variable: {}", x);
                    println!("Env: {:?}", env);
                    panic!()
                }
            }
        },
    }
}

fn eval_string(s: &str) -> ER {
    let tokens = s.split_ascii_whitespace().map(|s| Token::parse(s).unwrap()).collect_vec();
    let e = make_expr(&tokens).unwrap();
    println!("{:?}", e.to_string());
    let res = eval_expr(e, TreeMap::new());
    res
}





fn repl() {
    //let s = r#"B$ B$ L" B$ L# B$ v" B$ v# v# L# B$ v" B$ v# v# L" L# ? B= v# I! I" B$ L$ B+ B$ v" v$ B$ v" v$ B- v# I" I%"#;
    let s = fs::read_to_string("readme.icfp").unwrap();
    //let s = r#"B$ B$ L# L$ v# B. SB%,,/ S}Q/2,$_ IK"#;
    //let s = r#"B$ L# B$ L" B+ v" v" B* I$ I# v8"#;
    let e = eval_string(&s);
    match e.as_ref() {
        EvalResult::Str(s) => fs::write("readme_out.txt", s).unwrap(),
        _ => panic!()
    }
    //println!("{:?}", e);
    return;

    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let s = line.unwrap();
        let e = eval_string(&s);
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
    let e = eval_string(&expr);
    println!("{:?}", e);
    match e.as_ref() {
        EvalResult::Str(s) => {
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
    let e = eval_string(&expr);
    //println!("{:?}", e);
    match e.as_ref() {
        EvalResult::Str(s) => {
            s.clone()
        },
        _ => panic!()
    }
}

fn download_lambdaman() {
    for i in 7..=21 {
        let t = communicate_msg(&format!("get lambdaman{}", i));
        let fname = format!("../../data/lambdaman/{:02}.in", i);
        fs::write(&fname, t).unwrap();
        println!("Saved {}", fname);
        thread::sleep(Duration::from_secs(5));
    }
}

fn main_inner() {
    repl()
    //communicate()
    //download_lambdaman()
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