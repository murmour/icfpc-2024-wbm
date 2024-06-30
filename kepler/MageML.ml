
module S = String
module L = List
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
    | App [] ->
        assert false
    | App [ Var "take"; a; b ] ->
        Take (expr env a, expr env b)
    | App [ Var "drop"; a; b ] ->
        Drop (expr env a, expr env b)
    | App [ Var "stoi"; a ] ->
        StoI (expr env a)
    | App [ Var "itos"; a ] ->
        ItoS (expr env a)
    | App (x :: xs) ->
        let e = ref (expr env x) in
        xs |> L.iter (fun x -> e := App (!e, expr env x));
        !e
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
        expr env (App [Fun ([(v, res_type)], TAny, body); rhs])
    | Let (v, args, res_type, rhs, body) ->
        let rhs = ML.Fun (args, res_type, rhs) in
        expr env (App [Fun ([(v, TAny)], TAny, body); rhs])
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
        App [Fun ([(v, res_type)], TAny, desugar xs); body]
    | (v, args, res_type, body) :: xs ->
        let rhs = ML.Fun (args, res_type, body) in
        App [Fun ([(v, TAny)], TAny, desugar xs); rhs]
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

let parse_mage (s: string) : ML.program =
  match parse s with
    | Ok p -> p
    | Error msg ->
        Printf.eprintf "parse error: %s\n" msg;
        exit 1

let compile_mage (p: ML.program) : Kepler.expr =
  match compile_program p with
    | Ok e -> e
    | Error msg ->
        Printf.eprintf "compile error: %s\n" msg;
        exit 1

let parse_icfp (s: string) : Kepler.expr =
  match K.parse_expr s with
    | Ok r -> r
    | Error msg ->
        Printf.eprintf "parse error: %s\n" msg;
        exit 1

let eval_icfp (e: Kepler.expr) : Kepler.eval_res =
  match K.eval e with
    | Ok r -> r
    | Error msg ->
        Printf.eprintf "eval error: %s\n" msg;
        exit 1


let () =
  let mode = match Sys.argv.(1) with
    | "-mage_eval" -> `Mage_eval
    | "-mage_to_icfp" -> `Mage_to_icfp
    | "-mage_to_hum" -> `Mage_to_hum
    | "-mage_test_len" -> `Mage_test_len
    | "-icfp_eval" -> `Icfp_eval
    | "-icfp_to_hum" -> `Icfp_to_hum
    | etc ->
        failwith (sprintf "unknown mode: %s" etc)
  in
  let in_file = Sys.argv.(2) in
  let ch = open_in in_file in
  let s = really_input_string ch (in_channel_length ch) in

  match mode with
    | `Mage_eval ->
        let mage_ast = parse_mage s in
        let icfp_expr = compile_mage mage_ast in
        let eval_res = eval_icfp icfp_expr in
        output_string stdout (K.print_res eval_res);
        exit 0

    | `Mage_to_icfp ->
        let mage_ast = parse_mage s in
        let icfp_expr = compile_mage mage_ast in
        output_string stdout (K.print_icfp icfp_expr);
        exit 0

    | `Mage_to_hum ->
        let mage_ast = parse_mage s in
        let icfp_expr = compile_mage mage_ast in
        output_string stdout (K.print_expr icfp_expr);
        exit 0

    | `Mage_test_len ->
        let mage_ast = parse_mage s in
        let icfp_expr = compile_mage mage_ast in
        let eval_res = eval_icfp icfp_expr in
        let s1 = K.print_res eval_res in
        let s2 = K.print_icfp icfp_expr in
        Printf.printf "string len: %d, icfp len: %d" (S.length s1) (S.length s2);
        exit 0

    | `Icfp_eval ->
        let icfp_expr = parse_icfp s in
        let eval_res = eval_icfp icfp_expr in
        output_string stdout (K.print_res eval_res);
        exit 0

    | `Icfp_to_hum ->
        let icfp_expr = parse_icfp s in
        output_string stdout (K.print_expr icfp_expr);
        exit 0
