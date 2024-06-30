
module S = String
module SM = Map.Make (String)

module K = Kepler
module ML = Peg_MageML


let sprintf = Printf.sprintf


(* Compiler
   -------------------------------------------------------------------------- *)

type env = { map: K.var SM.t; ct: int }

let compile_expr (env: env) (e: ML.expr) : (K.expr, string) result =
  let exception Err of string in
  let err s = raise (Err s) in

  let rec expr (env: env) : ML.expr -> K.expr = function
    | True -> B true
    | False -> B false
    | String s -> S s
    | Int s -> I (int_of_string s)
    | Neg e -> Neg (expr env e)
    | Not e -> Not (expr env e)
    | Add (a, b) -> Add (expr env a, expr env b)
    | Sub (a, b) -> Sub (expr env a, expr env b)
    | Mul (a, b) -> Mul (expr env a, expr env b)
    | Div (a, b) -> Div (expr env a, expr env b)
    | Mod (a, b) -> Mod (expr env a, expr env b)
    | Lt (a, b) -> Lt (expr env a, expr env b)
    | Gt (a, b) -> Gt (expr env a, expr env b)
    | Eq (a, b) -> Eq (expr env a, expr env b)
    | Or (a, b) -> Or (expr env a, expr env b)
    | And (a, b) -> And (expr env a, expr env b)
    | Conc (a, b) -> Conc (expr env a, expr env b)
    | If (cond, thn, els) -> If (expr env cond, expr env thn, expr env els)
    | App (a, b) -> App (expr env a, expr env b)
    | Fun (args, _res_type, body) ->
        let rec aux env = function
          | [] ->
              expr env body
          | (x, _type) :: xs ->
              let v = env.ct in
              let env = { map = SM.add x env.ct env.map; ct = v+1 } in
              Lam (v, aux env xs)
        in
        aux env args
    | Let (v, [], res_type, rhs, body) ->
        expr env (App (Fun ([(v, res_type)], TAny, body), rhs))
    | Let (v, args, res_type, rhs, body) ->
        let rhs = ML.Fun (args, res_type, rhs) in
        expr env (App (Fun ([(v, TAny)], TAny, body), rhs))
    | Var v ->
        try
          Var (env.map |> SM.find v)
        with Not_found ->
          err (sprintf "Invalid reference: %s" v)
  in
  try
    Ok (expr env e)
  with Err s ->
    Error s


let compile_program (p: ML.program) : (K.expr, string) result =
  let exception Err of string in
  let err s = raise (Err s) in

  let env = { map = SM.empty; ct = 0 } in
  let rec desugar: ML.decl list -> ML.expr = function
    | (v, [], res_type, body) :: xs ->
        App (Fun ([(v, res_type)], TAny, desugar xs), body)
    | (v, args, res_type, body) :: xs ->
        let rhs = ML.Fun (args, res_type, body) in
        App (Fun ([(v, TAny)], TAny, desugar xs), rhs)
    | [] ->
        try
          Var "magic"
        with Not_found ->
          err "Magic not found!"
  in

  try
    let e = desugar p in
    compile_expr env e
  with Err s ->
    Error s


(* Parsing
   -------------------------------------------------------------------------- *)

let parse (s: string) : (Peg_MageML.program, string) result =
  let s = CharStream.from_string s in
  match Peg_MageML.program s 0 with
    | Ok (p, _) ->
        Ok p
    | Error (tags, idx) ->
        let p = CharStream.get_pos s idx in
        let exp = S.concat ", " tags in
        Error (sprintf "line %d, col %d, expected: %s" p.line p.column exp)


(* CLI
   -------------------------------------------------------------------------- *)

let () =
  let mode = match Sys.argv.(1) with
    | "-print_icfp" -> `Print_icfp
    | "-print_kepler" -> `Print_kepler
    | "-check_length" -> `Check_length
    | "-eval" -> `Eval
    | etc ->
        failwith (sprintf "unknown mode: %s" etc)
  in
  let in_file = Sys.argv.(2) in
  let ch = open_in in_file in
  let s = really_input_string ch (in_channel_length ch) in
  let ast =
    match parse s with
      | Ok p -> p
      | Error msg ->
          Printf.eprintf "parse error: %s\n" msg;
          exit 1
  in
  let kepler_expr =
    match compile_program ast with
      | Ok e -> e
      | Error msg ->
          Printf.eprintf "compile error: %s\n" msg;
          exit 1
  in
  match mode with
    | `Eval ->
        let eval_res =
          match K.eval kepler_expr with
            | Ok r -> r
            | Error msg ->
                Printf.eprintf "eval error: %s\n" msg;
                exit 1
        in
        output_string stdout (K.print_res eval_res);
        exit 0

    | `Print_kepler ->
        output_string stdout (K.print_expr kepler_expr);
        exit 0

    | `Print_icfp ->
        output_string stdout (K.print_icfp kepler_expr);
        exit 0

    | `Check_length ->
        let eval_res =
          match K.eval kepler_expr with
            | Ok r -> r
            | Error msg ->
                Printf.eprintf "eval error: %s\n" msg;
                exit 1
        in
        let s1 = K.print_res eval_res in
        let s2 = K.print_icfp kepler_expr in
        Printf.printf "string len: %d, icfp len: %d" (S.length s1) (S.length s2);
        exit 0
