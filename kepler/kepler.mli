
type var = int

type expr =
  | B of bool
  | I of int
  | S of string
  | Neg of int_expr
  | Not of bool_expr
  | StoI of string_expr
  | ItoS of int_expr
  | Add of int_expr * int_expr
  | Sub of int_expr * int_expr
  | Mul of int_expr * int_expr
  | Div of int_expr * int_expr
  | Mod of int_expr * int_expr
  | Lt of int_expr * int_expr
  | Gt of int_expr * int_expr
  | Eq of expr(* BIS *) * expr(* BIS *)
  | Or of bool_expr * bool_expr
  | And of bool_expr * bool_expr
  | Conc of string_expr * string_expr
  | Take of int_expr * string_expr
  | Drop of int_expr * string_expr
  | If of bool_expr * expr * expr
  | App of expr * expr
  | Lam of var * expr
  | Var of var
  | Trace of var
  | Panic of expr

and int_expr = expr
and string_expr = expr
and bool_expr = expr

type eval_res


val encode_string: string -> string
val parse_expr: string -> (expr, string) result
val print_icfp: expr -> string
val print_expr: expr -> string
val eval: expr -> (eval_res, string) result
val print_res: eval_res -> string
val print_raw_res: eval_res -> string
