
module HT = Hashtbl
module BF = Buffer
module S = String
module IM = Map.Make (Int)

let sprintf = Printf.sprintf


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

and int_expr = expr
and string_expr = expr
and bool_expr = expr


(* Encoding
   -------------------------------------------------------------------------- *)

let string_char_map =
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789\
   !\"#$%&'()*+,-./:;<=>?@[\\]^_`|~\ \n"

let decode_string (s: string) : string =
  s |> S.map (fun c ->
    let i = Char.code c in
    assert (i >= 33 && i <= 126);
    string_char_map.[i - 33])

let encode_string: string -> string =
  let h = HT.create 100 in
  string_char_map |> S.iteri (fun i c ->
    HT.add h c (Char.chr (i + 33)));
  fun s ->
    s |> S.map (fun c ->
      try HT.find h c with Not_found -> assert false)

let decode_int (s: string) : int =
  let i = ref 0 in
  s |> S.iter (fun c -> i := !i*94 + (Char.code c) - 33);
  !i

let encode_int (i: int) : string =
  assert (i >= 0);
  let b = BF.create 20 in
  let n = ref i in
  while !n > 0 do
    let i = !n mod 94 in
    BF.add_char b (Char.chr (i + 33));
    n := !n / 94
  done;
  let s = Buffer.contents b in
  let len = S.length s in
  S.init len (fun i -> String.get s (len-i-1))


(* Parsing
   -------------------------------------------------------------------------- *)

let parse_expr (s: string) : (expr, string) result =
  let exception Err of string in
  let err s = raise (Err s) in

  let i = ref 0 in
  let s = s ^ "\000" in

  let rec expr () : expr =
    match s.[!i] with
      | ' ' -> incr i; expr ()
      | 'T' -> incr i; B true
      | 'F' -> incr i; B false
      | 'I' -> incr i; I (int ())
      | 'S' -> incr i; S (decode_string (body ()))
      | 'U' ->
          incr i;
          begin match s.[!i] with
            | '-' -> incr i; Neg (expr ())
            | '!' -> incr i; Not (expr ())
            | '#' -> incr i; StoI (expr ())
            | '$' -> incr i; ItoS (expr ())
            | c -> err (sprintf "unexpected %c at %d\n" c !i)
          end
      | 'B' ->
          incr i;
          begin match s.[!i] with
            | '+' -> incr i; let l = expr () in let r = expr () in Add (l, r)
            | '-' -> incr i; let l = expr () in let r = expr () in Sub (l, r)
            | '*' -> incr i; let l = expr () in let r = expr () in Mul (l, r)
            | '/' -> incr i; let l = expr () in let r = expr () in Div (l, r)
            | '%' -> incr i; let l = expr () in let r = expr () in Mod (l, r)
            | '<' -> incr i; let l = expr () in let r = expr () in Lt (l, r)
            | '>' -> incr i; let l = expr () in let r = expr () in Gt (l, r)
            | '=' -> incr i; let l = expr () in let r = expr () in Eq (l, r)
            | '|' -> incr i; let l = expr () in let r = expr () in Or (l, r)
            | '&' -> incr i; let l = expr () in let r = expr () in And (l, r)
            | '.' -> incr i; let l = expr () in let r = expr () in Conc (l, r)
            | 'T' -> incr i; let l = expr () in let r = expr () in Take (l, r)
            | 'D' -> incr i; let l = expr () in let r = expr () in Drop (l, r)
            | '$' -> incr i; let l = expr () in let r = expr () in App (l, r)
            | c -> err (sprintf "unexpected %c at %d\n" c !i)
          end
      | '?' ->
          incr i;
          let cond = expr () in
          let thn = expr () in
          let els = expr () in
          If (cond, thn, els)
      | 'L' ->
          incr i;
          let var = int () in
          let body = expr () in
          Lam (var, body)
      | 'v' ->
          incr i;
          Var (int ())
      | c ->
          err (sprintf "unexpected %c at %d\n" c !i)

  and int () : int =
    decode_int (body ())

  and body () : string =
    let b = BF.create 20 in
    while match s.[!i] with '!'..'~' -> true | _ -> false do
      BF.add_char b s.[!i];
      incr i;
    done;
    BF.contents b
  in

  try Ok (expr ()) with Err s -> Error s


(* Printing
   -------------------------------------------------------------------------- *)

let rec print_expr (e: expr) : string =
  match e with
    | B true -> "t"
    | B false -> "f"
    | I i -> string_of_int i
    | S s -> sprintf "\"%s\"" s
    | Neg e -> sprintf "-(%s)" (print_expr e)
    | Not e -> sprintf "not %s" (print_expr e)
    | StoI e -> sprintf "stoi %s" (print_expr e)
    | ItoS e -> sprintf "itos %s" (print_expr e)
    | Add (a, b) -> sprintf "%s + %s" (print_expr a) (print_expr b)
    | Sub (a, b) -> sprintf "%s - %s" (print_expr a) (print_expr b)
    | Mul (a, b) -> sprintf "%s * %s" (print_expr a) (print_expr b)
    | Div (a, b) -> sprintf "%s / %s" (print_expr a) (print_expr b)
    | Mod (a, b) -> sprintf "%s mod %s" (print_expr a) (print_expr b)
    | Lt (a, b) -> sprintf "%s < %s" (print_expr a) (print_expr b)
    | Gt (a, b) -> sprintf "%s > %s" (print_expr a) (print_expr b)
    | Eq (a, b) -> sprintf "%s = %s" (print_expr a) (print_expr b)
    | Or (a, b) -> sprintf "%s || %s" (print_expr a) (print_expr b)
    | And (a, b) -> sprintf "%s && %s" (print_expr a) (print_expr b)
    | Conc (a, b) -> sprintf "%s ^ %s" (print_expr a) (print_expr b)
    | Take (a, b) -> sprintf "take %s %s" (print_expr a) (print_expr b)
    | Drop (a, b) -> sprintf "drop %s %s" (print_expr a) (print_expr b)
    | If (cond, thn, els) ->
        sprintf "if %s %s %s" (print_expr cond) (print_expr thn) (print_expr els)
    | App (a, b) -> sprintf "%s(%s)" (print_expr a) (print_expr b)
    | Lam (v, e) -> sprintf "(fun v%d -> %s)" v (print_expr e)
    | Var v -> sprintf "v%d" v


(* Evaluating
   -------------------------------------------------------------------------- *)

type eval_res =
  | B of bool
  | I of int
  | S of string
  | Lam of var * expr * eval_res Lazy.t IM.t


let print_res (r: eval_res) : string =
  match r with
    | B b -> sprintf "%b" b
    | I i -> sprintf "%d" i
    | S s -> sprintf "\"%s\"" s
    | Lam (v, _, _) -> sprintf "fun v%i -> ..." v


let print_raw_res (r: eval_res) : string =
  match r with
    | B b -> sprintf "%b" b
    | I i -> sprintf "%d" i
    | S s -> s
    | Lam (v, _, _) -> sprintf "fun v%i -> ..." v


let eval (e: expr) : (eval_res, string) result =
  let exception Err of string in
  let pp = print_res in
  let err s = raise (Err s) in

  let rec eval (env: eval_res Lazy.t IM.t) (e: expr) : eval_res =
    match e with
      | B b -> B b
      | I i -> I i
      | S s -> S s
      | Neg e ->
          begin match eval env e with
            | I i -> I (-i)
            | r -> err (sprintf "Neg got %s" (pp r))
          end
      | Not e ->
          begin match eval env e with
            | B b -> B (not b)
            | r -> err (sprintf "Not got %s" (pp r))
          end
      | StoI e ->
          begin match eval env e with
            | S s -> I (decode_int (encode_string s))
            | r -> err (sprintf "StoI got %s" (pp r))
          end
      | ItoS e ->
          begin match eval env e with
            | I i -> S (decode_string (encode_int i))
            | r -> err (sprintf "ItoS got %s" (pp r))
          end
      | Add (a, b) ->
          begin match (eval env a, eval env b) with
            | (I a, I b) -> I (a+b)
            | (a, b) -> err (sprintf "Add got %s, %s" (pp a) (pp b))
          end
      | Sub (a, b) ->
          begin match (eval env a, eval env b) with
            | (I a, I b) -> I (a-b)
            | (a, b) -> err (sprintf "Sub got %s, %s" (pp a) (pp b))
          end
      | Mul (a, b) ->
          begin match (eval env a, eval env b) with
            | (I a, I b) -> I (a*b)
            | (a, b) -> err (sprintf "Mul got %s, %s" (pp a) (pp b))
          end
      | Div (a, b) ->
          begin match (eval env a, eval env b) with
            | (I a, I b) -> I (a/b)
            | (a, b) -> err (sprintf "Div got %s, %s" (pp a) (pp b))
          end
      | Mod (a, b) ->
          begin match (eval env a, eval env b) with
            | (I a, I b) -> I (a mod b)
            | (a, b) -> err (sprintf "Mod got %s, %s" (pp a) (pp b))
          end
      | Lt (a, b) ->
          begin match (eval env a, eval env b) with
            | (I a, I b) -> B (a < b)
            | (a, b) -> err (sprintf "Lt got %s, %s" (pp a) (pp b))
          end
      | Gt (a, b) ->
          begin match (eval env a, eval env b) with
            | (I a, I b) -> B (a > b)
            | (a, b) -> err (sprintf "Gt got %s, %s" (pp a) (pp b))
          end
      | Eq (a, b) ->
          begin match (eval env a, eval env b) with
            | (B a, B b) -> B (a = b)
            | (I a, I b) -> B (a = b)
            | (S a, S b) -> B (a = b)
            | (a, b) -> err (sprintf "Eq got %s, %s" (pp a) (pp b))
          end
      | Or (a, b) ->
          (* todo: short-curcuit (in which direction?) *)
          begin match (eval env a, eval env b) with
            | (B a, B b) -> B (a || b)
            | (a, b) -> err (sprintf "Or got %s, %s" (pp a) (pp b))
          end
      | And (a, b) ->
          (* todo: short-curcuit (in which direction?) *)
          begin match (eval env a, eval env b) with
            | (B a, B b) -> B (a && b)
            | (a, b) -> err (sprintf "Or got %s, %s" (pp a) (pp b))
          end
      | Conc (a, b) ->
          begin match (eval env a, eval env b) with
            | (S a, S b) -> S (a ^ b)
            | (a, b) -> err (sprintf "Conc got %s, %s" (pp a) (pp b))
          end
      | Take (a, b) ->
          begin match (eval env a, eval env b) with
            | (I i, S s) -> S (String.sub s 0 i)
            | (a, b) -> err (sprintf "Take got %s, %s" (pp a) (pp b))
          end
      | Drop (a, b) ->
          begin match (eval env a, eval env b) with
            | (I i, S s) -> S (String.sub s i (S.length s - i))
            | (a, b) -> err (sprintf "Take got %s, %s" (pp a) (pp b))
          end
      | If (cond, thn, els) ->
          begin match eval env cond with
            | (B true) -> eval env thn
            | (B false) -> eval env els
            | cond -> err (sprintf "If got %s" (pp cond))
          end
      | Lam (v, body) ->
          Lam (v, body, env)
      | Var v ->
          begin match IM.find_opt v env with
            | Some e -> Lazy.force e
            | None -> err (sprintf "Unbound var: %d" v)
          end
      | App (a, b) ->
          begin match eval env a with
            | Lam (v, body, lam_env) ->
                let body_env = IM.add v (lazy (eval env b)) lam_env in
                eval body_env body
            | a -> err (sprintf "Lam got %s" (pp a))
          end
  in

  try Ok (eval IM.empty e) with Err s -> Error s
