
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
  | UNeg of int_expr
  | UNot of bool_expr
  | StoI of string_expr
  | ItoS of int_expr
  | BAdd of int_expr * int_expr
  | BSub of int_expr * int_expr
  | BMul of int_expr * int_expr
  | BDiv of int_expr * int_expr
  | BMod of int_expr * int_expr
  | BLt of int_expr * int_expr
  | BGt of int_expr * int_expr
  | BEq of expr(* BIS *) * expr(* BIS *)
  | BOr of bool_expr * bool_expr
  | BAnd of bool_expr * bool_expr
  | BConc of string_expr * string_expr
  | Take of int_expr * string_expr
  | Drop of int_expr * string_expr
  | If of { cond: bool_expr; thn: expr; els: expr }
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
            | '-' -> incr i; UNeg (expr ())
            | '!' -> incr i; UNot (expr ())
            | '#' -> incr i; StoI (expr ())
            | '$' -> incr i; ItoS (expr ())
            | c -> err (sprintf "unexpected %c at %d\n" c !i)
          end
      | 'B' ->
          incr i;
          begin match s.[!i] with
            | '+' -> incr i; let l = expr () in let r = expr () in BAdd (l, r)
            | '-' -> incr i; let l = expr () in let r = expr () in BSub (l, r)
            | '*' -> incr i; let l = expr () in let r = expr () in BMul (l, r)
            | '/' -> incr i; let l = expr () in let r = expr () in BDiv (l, r)
            | '%' -> incr i; let l = expr () in let r = expr () in BMod (l, r)
            | '<' -> incr i; let l = expr () in let r = expr () in BLt (l, r)
            | '>' -> incr i; let l = expr () in let r = expr () in BGt (l, r)
            | '=' -> incr i; let l = expr () in let r = expr () in BEq (l, r)
            | '|' -> incr i; let l = expr () in let r = expr () in BOr (l, r)
            | '&' -> incr i; let l = expr () in let r = expr () in BAnd (l, r)
            | '.' -> incr i; let l = expr () in let r = expr () in BConc (l, r)
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
          If { cond; thn; els }
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
    | UNeg e -> sprintf "-(%s)" (print_expr e)
    | UNot e -> sprintf "not %s" (print_expr e)
    | StoI e -> sprintf "stoi %s" (print_expr e)
    | ItoS e -> sprintf "itos %s" (print_expr e)
    | BAdd (a, b) -> sprintf "%s + %s" (print_expr a) (print_expr b)
    | BSub (a, b) -> sprintf "%s - %s" (print_expr a) (print_expr b)
    | BMul (a, b) -> sprintf "%s * %s" (print_expr a) (print_expr b)
    | BDiv (a, b) -> sprintf "%s / %s" (print_expr a) (print_expr b)
    | BMod (a, b) -> sprintf "%s mod %s" (print_expr a) (print_expr b)
    | BLt (a, b) -> sprintf "%s < %s" (print_expr a) (print_expr b)
    | BGt (a, b) -> sprintf "%s > %s" (print_expr a) (print_expr b)
    | BEq (a, b) -> sprintf "%s = %s" (print_expr a) (print_expr b)
    | BOr (a, b) -> sprintf "%s || %s" (print_expr a) (print_expr b)
    | BAnd (a, b) -> sprintf "%s && %s" (print_expr a) (print_expr b)
    | BConc (a, b) -> sprintf "%s ^ %s" (print_expr a) (print_expr b)
    | Take (a, b) -> sprintf "take %s %s" (print_expr a) (print_expr b)
    | Drop (a, b) -> sprintf "drop %s %s" (print_expr a) (print_expr b)
    | If { cond; thn; els } ->
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
      | UNeg e ->
          begin match eval env e with
            | I i -> I (-i)
            | r -> err (sprintf "UNeg got %s" (pp r))
          end
      | UNot e ->
          begin match eval env e with
            | B b -> B (not b)
            | r -> err (sprintf "UNot got %s" (pp r))
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
      | BAdd (a, b) ->
          begin match (eval env a, eval env b) with
            | (I a, I b) -> I (a+b)
            | (a, b) -> err (sprintf "BAdd got %s, %s" (pp a) (pp b))
          end
      | BSub (a, b) ->
          begin match (eval env a, eval env b) with
            | (I a, I b) -> I (a-b)
            | (a, b) -> err (sprintf "BSub got %s, %s" (pp a) (pp b))
          end
      | BMul (a, b) ->
          begin match (eval env a, eval env b) with
            | (I a, I b) -> I (a*b)
            | (a, b) -> err (sprintf "BMul got %s, %s" (pp a) (pp b))
          end
      | BDiv (a, b) ->
          begin match (eval env a, eval env b) with
            | (I a, I b) -> I (a/b)
            | (a, b) -> err (sprintf "BDiv got %s, %s" (pp a) (pp b))
          end
      | BMod (a, b) ->
          begin match (eval env a, eval env b) with
            | (I a, I b) -> I (a mod b)
            | (a, b) -> err (sprintf "BMod got %s, %s" (pp a) (pp b))
          end
      | BLt (a, b) ->
          begin match (eval env a, eval env b) with
            | (I a, I b) -> B (a < b)
            | (a, b) -> err (sprintf "BLt got %s, %s" (pp a) (pp b))
          end
      | BGt (a, b) ->
          begin match (eval env a, eval env b) with
            | (I a, I b) -> B (a > b)
            | (a, b) -> err (sprintf "BGt got %s, %s" (pp a) (pp b))
          end
      | BEq (a, b) ->
          begin match (eval env a, eval env b) with
            | (B a, B b) -> B (a = b)
            | (I a, I b) -> B (a = b)
            | (S a, S b) -> B (a = b)
            | (a, b) -> err (sprintf "BEq got %s, %s" (pp a) (pp b))
          end
      | BOr (a, b) ->
          (* todo: short-curcuit (in which direction?) *)
          begin match (eval env a, eval env b) with
            | (B a, B b) -> B (a || b)
            | (a, b) -> err (sprintf "BOr got %s, %s" (pp a) (pp b))
          end
      | BAnd (a, b) ->
          (* todo: short-curcuit (in which direction?) *)
          begin match (eval env a, eval env b) with
            | (B a, B b) -> B (a && b)
            | (a, b) -> err (sprintf "BOr got %s, %s" (pp a) (pp b))
          end
      | BConc (a, b) ->
          begin match (eval env a, eval env b) with
            | (S a, S b) -> S (a ^ b)
            | (a, b) -> err (sprintf "BConc got %s, %s" (pp a) (pp b))
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
      | If { cond; thn; els } ->
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
