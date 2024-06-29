(* this code is produced by magic! do not edit! *)

type var = string

type type_ =
  | TAny
  | TId of var
  | TFun of var * type_

type typed_arg = var * type_

type expr =
  | True
  | False
  | String of string
  | Int of string
  | Neg of expr
  | Not of expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Mod of expr * expr
  | Lt of expr * expr
  | Gt of expr * expr
  | Eq of expr * expr
  | Or of expr * expr
  | And of expr * expr
  | Conc of expr * expr
  | If of expr * expr * expr
  | App of expr * expr
  | Fun of typed_arg list * type_ * expr
  | Let of var * typed_arg list * type_ * expr * expr
  | Var of var

type decl = var * typed_arg list * type_ * expr

type program = decl list


module S = CharStream
type 'a parse_result = S of 'a * int | F
type ctx = { s: S.t; mutable maxidx: int; mutable tags: string list }
let mark _c _i tag =
  if _i = _c.maxidx then
    _c.tags <- tag :: _c.tags
  else if _i > _c.maxidx then
    (_c.maxidx <- _i; _c.tags <- [ tag ])
  else ()
let rec gDE _c _i =
  match ws _c _i with F -> F | S (_, _i) ->
  gDD _c _i
and gDD _c _i =
  match gDC _c _i with F -> F | S (r1, _i) ->
  match gDB _c _i with F -> F | S (_, _i) ->
  S (r1, _i)
and gDC _c _i =
  let rec iter _i =
    match decl _c _i with | F -> ([], _i) | S (r, _i) ->
    let (l, _i) = iter _i in (r :: l, _i)
  in
  let (l, _i) = iter _i in
  S (l, _i)
and gDB _c _i =
  if _i >= S.length _c.s then S ((), _i) else (mark _c _i "eof"; F)
and gDA _c _i =
  match gC' _c _i with F -> (mark _c _i "decl"; F) | s -> s
and gC' _c _i =
  match gC_ _c _i with F -> F | S (r1, _i) ->
  match gC1 _c _i with F -> F | S (_, _i) ->
  S (r1, _i)
and gC_ _c _i =
  match gC5 _c _i with F -> F | S (r0, _i) ->
  match gC6 _c _i with F -> F | S (r1, _i) ->
  match res_type _c _i with F -> F | S (r2, _i) ->
  match gC9 _c _i with F -> F | S (r3, _i) ->
  S ((r0, r1, r2, r3), _i)
and gC9 _c _i =
  match gC8 _c _i with F -> F | S (_, _i) ->
  expr _c _i
and gC8 _c _i =
  match gC7 _c _i with F -> F | S (r1, _i) ->
  match ws _c _i with F -> F | S (_, _i) ->
  S (r1, _i)
and gC7 _c _i =
  if S.match_char '=' _c.s _i then S ((), _i+1) else F
and gC6 _c _i =
  let rec iter _i =
    match typed_arg _c _i with | F -> ([], _i) | S (r, _i) ->
    let (l, _i) = iter _i in (r :: l, _i)
  in
  let (l, _i) = iter _i in
  S (l, _i)
and gC5 _c _i =
  match gC4 _c _i with F -> F | S (_, _i) ->
  id _c _i
and gC4 _c _i =
  if not (S.match_string "let" _c.s _i) then F else let _i = _i+3 in
  gC3 _c _i
and gC3 _c _i =
  if gC2 _c _i <> F then F else ws _c _i
and gC2 _c _i =
  match S.read_char _c.s _i with Some c when (c >= 'a' && c <= 'z') || ((c >= 'A' && c <= 'Z') || ((c >= '0' && c <= '9') || (c = '_'))) -> S ((), _i+1) | _ -> F
and gC1 _c _i =
  match gC0 _c _i with F -> F | S (r1, _i) ->
  match ws _c _i with F -> F | S (_, _i) ->
  S (r1, _i)
and gC0 _c _i =
  if S.match_char ';' _c.s _i then S ((), _i+1) else F
and gCz _c _i =
  match expr1 _c _i with F -> F | S (lhs, _i) -> (match (match S (lhs, _i) with F -> F | S (r0, _i) ->
  match (match (match (if S.match_string "&&" _c.s _i then S ((), _i+2) else F) with F -> F | S (r1, _i) ->
  match ws _c _i with F -> F | S (_, _i) ->
  S (r1, _i)) with F -> F | S (_, _i) ->
  expr0 _c _i) with F -> F | S (r1, _i) ->
  S (And (r0, r1), _i)) with F -> (match (match S (lhs, _i) with F -> F | S (r0, _i) ->
  match (match (match (if S.match_string "||" _c.s _i then S ((), _i+2) else F) with F -> F | S (r1, _i) ->
  match ws _c _i with F -> F | S (_, _i) ->
  S (r1, _i)) with F -> F | S (_, _i) ->
  expr0 _c _i) with F -> F | S (r1, _i) ->
  S (Or (r0, r1), _i)) with F -> S (lhs, _i) | s -> s) | s -> s)
and gCy _c _i =
  match expr2 _c _i with F -> F | S (lhs, _i) -> (match (match S (lhs, _i) with F -> F | S (r0, _i) ->
  match (match (match (if S.match_char '>' _c.s _i then S ((), _i+1) else F) with F -> F | S (r1, _i) ->
  match ws _c _i with F -> F | S (_, _i) ->
  S (r1, _i)) with F -> F | S (_, _i) ->
  expr1 _c _i) with F -> F | S (r1, _i) ->
  S (Gt (r0, r1), _i)) with F -> (match (match S (lhs, _i) with F -> F | S (r0, _i) ->
  match (match (match (if S.match_char '<' _c.s _i then S ((), _i+1) else F) with F -> F | S (r1, _i) ->
  match ws _c _i with F -> F | S (_, _i) ->
  S (r1, _i)) with F -> F | S (_, _i) ->
  expr1 _c _i) with F -> F | S (r1, _i) ->
  S (Lt (r0, r1), _i)) with F -> (match (match S (lhs, _i) with F -> F | S (r0, _i) ->
  match (match (match (if S.match_char '=' _c.s _i then S ((), _i+1) else F) with F -> F | S (r1, _i) ->
  match ws _c _i with F -> F | S (_, _i) ->
  S (r1, _i)) with F -> F | S (_, _i) ->
  expr1 _c _i) with F -> F | S (r1, _i) ->
  S (Eq (r0, r1), _i)) with F -> (match (match S (lhs, _i) with F -> F | S (r0, _i) ->
  match (match (match (if S.match_char '^' _c.s _i then S ((), _i+1) else F) with F -> F | S (r1, _i) ->
  match ws _c _i with F -> F | S (_, _i) ->
  S (r1, _i)) with F -> F | S (_, _i) ->
  expr1 _c _i) with F -> F | S (r1, _i) ->
  S (Conc (r0, r1), _i)) with F -> S (lhs, _i) | s -> s) | s -> s) | s -> s) | s -> s)
and gCx _c _i =
  match expr3 _c _i with F -> F | S (lhs, _i) -> (match (match S (lhs, _i) with F -> F | S (r0, _i) ->
  match (match (match (if S.match_char '+' _c.s _i then S ((), _i+1) else F) with F -> F | S (r1, _i) ->
  match ws _c _i with F -> F | S (_, _i) ->
  S (r1, _i)) with F -> F | S (_, _i) ->
  expr2 _c _i) with F -> F | S (r1, _i) ->
  S (Add (r0, r1), _i)) with F -> (match (match S (lhs, _i) with F -> F | S (r0, _i) ->
  match (match (match (if S.match_char '-' _c.s _i then S ((), _i+1) else F) with F -> F | S (r1, _i) ->
  match ws _c _i with F -> F | S (_, _i) ->
  S (r1, _i)) with F -> F | S (_, _i) ->
  expr2 _c _i) with F -> F | S (r1, _i) ->
  S (Sub (r0, r1), _i)) with F -> S (lhs, _i) | s -> s) | s -> s)
and gCw _c _i =
  match expr4 _c _i with F -> F | S (lhs, _i) -> (match (match S (lhs, _i) with F -> F | S (r0, _i) ->
  match (match (match (if S.match_char '*' _c.s _i then S ((), _i+1) else F) with F -> F | S (r1, _i) ->
  match ws _c _i with F -> F | S (_, _i) ->
  S (r1, _i)) with F -> F | S (_, _i) ->
  expr3 _c _i) with F -> F | S (r1, _i) ->
  S (Mul (r0, r1), _i)) with F -> (match (match S (lhs, _i) with F -> F | S (r0, _i) ->
  match (match (match (if S.match_char '/' _c.s _i then S ((), _i+1) else F) with F -> F | S (r1, _i) ->
  match ws _c _i with F -> F | S (_, _i) ->
  S (r1, _i)) with F -> F | S (_, _i) ->
  expr3 _c _i) with F -> F | S (r1, _i) ->
  S (Div (r0, r1), _i)) with F -> (match (match S (lhs, _i) with F -> F | S (r0, _i) ->
  match (match (match (if S.match_char '%' _c.s _i then S ((), _i+1) else F) with F -> F | S (r1, _i) ->
  match ws _c _i with F -> F | S (_, _i) ->
  S (r1, _i)) with F -> F | S (_, _i) ->
  expr3 _c _i) with F -> F | S (r1, _i) ->
  S (Mod (r0, r1), _i)) with F -> S (lhs, _i) | s -> s) | s -> s) | s -> s)
and gCv _c _i =
  match gCu _c _i with F -> gCq _c _i | s -> s
and gCu _c _i =
  match gCt _c _i with F -> F | S (r0, _i) ->
  S (Neg (r0), _i)
and gCt _c _i =
  match gCs _c _i with F -> F | S (_, _i) ->
  expr4 _c _i
and gCs _c _i =
  match gCr _c _i with F -> F | S (r1, _i) ->
  match ws _c _i with F -> F | S (_, _i) ->
  S (r1, _i)
and gCr _c _i =
  if S.match_char '-' _c.s _i then S ((), _i+1) else F
and gCq _c _i =
  match gCp _c _i with F -> expr5 _c _i | s -> s
and gCp _c _i =
  match gCo _c _i with F -> F | S (r0, _i) ->
  S (Not (r0), _i)
and gCo _c _i =
  match gCn _c _i with F -> F | S (_, _i) ->
  expr4 _c _i
and gCn _c _i =
  if not (S.match_string "not" _c.s _i) then F else let _i = _i+3 in
  gCm _c _i
and gCm _c _i =
  if gCl _c _i <> F then F else ws _c _i
and gCl _c _i =
  match S.read_char _c.s _i with Some c when (c >= 'a' && c <= 'z') || ((c >= 'A' && c <= 'Z') || ((c >= '0' && c <= '9') || (c = '_'))) -> S ((), _i+1) | _ -> F
and gCk _c _i =
  match expr6 _c _i with F -> F | S (lhs, _i) -> (match (match S (lhs, _i) with F -> F | S (r0, _i) ->
  match expr5 _c _i with F -> F | S (r1, _i) ->
  S (App (r0, r1), _i)) with F -> S (lhs, _i) | s -> s)
and gCj _c _i =
  match gCi _c _i with F -> gCd _c _i | s -> s
and gCi _c _i =
  match gCh _c _i with F -> F | S (_, _i) ->
  gCe _c _i
and gCh _c _i =
  if not (S.match_string "true" _c.s _i) then F else let _i = _i+4 in
  gCg _c _i
and gCg _c _i =
  if gCf _c _i <> F then F else ws _c _i
and gCf _c _i =
  match S.read_char _c.s _i with Some c when (c >= 'a' && c <= 'z') || ((c >= 'A' && c <= 'Z') || ((c >= '0' && c <= '9') || (c = '_'))) -> S ((), _i+1) | _ -> F
and gCe _c _i =
  S (True, _i)
and gCd _c _i =
  match gCc _c _i with F -> gCX _c _i | s -> s
and gCc _c _i =
  match gCb _c _i with F -> F | S (_, _i) ->
  gCY _c _i
and gCb _c _i =
  if not (S.match_string "false" _c.s _i) then F else let _i = _i+5 in
  gCa _c _i
and gCa _c _i =
  if gCZ _c _i <> F then F else ws _c _i
and gCZ _c _i =
  match S.read_char _c.s _i with Some c when (c >= 'a' && c <= 'z') || ((c >= 'A' && c <= 'Z') || ((c >= '0' && c <= '9') || (c = '_'))) -> S ((), _i+1) | _ -> F
and gCY _c _i =
  S (False, _i)
and gCX _c _i =
  match gCW _c _i with F -> gCV _c _i | s -> s
and gCW _c _i =
  match lit_int _c _i with F -> F | S (r0, _i) ->
  S (Int (r0), _i)
and gCV _c _i =
  match gCU _c _i with F -> gCT _c _i | s -> s
and gCU _c _i =
  match lit_string _c _i with F -> F | S (r0, _i) ->
  S (String (r0), _i)
and gCT _c _i =
  match gCS _c _i with F -> gCR _c _i | s -> s
and gCS _c _i =
  match id _c _i with F -> F | S (r0, _i) ->
  S (Var (r0), _i)
and gCR _c _i =
  match gCQ _c _i with F -> gCD _c _i | s -> s
and gCQ _c _i =
  match gCH _c _i with F -> F | S (r0, _i) ->
  match gCL _c _i with F -> F | S (r1, _i) ->
  match gCP _c _i with F -> F | S (r2, _i) ->
  S (If (r0, r1, r2), _i)
and gCP _c _i =
  match gCO _c _i with F -> F | S (_, _i) ->
  expr _c _i
and gCO _c _i =
  if not (S.match_string "else" _c.s _i) then F else let _i = _i+4 in
  gCN _c _i
and gCN _c _i =
  if gCM _c _i <> F then F else ws _c _i
and gCM _c _i =
  match S.read_char _c.s _i with Some c when (c >= 'a' && c <= 'z') || ((c >= 'A' && c <= 'Z') || ((c >= '0' && c <= '9') || (c = '_'))) -> S ((), _i+1) | _ -> F
and gCL _c _i =
  match gCK _c _i with F -> F | S (_, _i) ->
  expr _c _i
and gCK _c _i =
  if not (S.match_string "then" _c.s _i) then F else let _i = _i+4 in
  gCJ _c _i
and gCJ _c _i =
  if gCI _c _i <> F then F else ws _c _i
and gCI _c _i =
  match S.read_char _c.s _i with Some c when (c >= 'a' && c <= 'z') || ((c >= 'A' && c <= 'Z') || ((c >= '0' && c <= '9') || (c = '_'))) -> S ((), _i+1) | _ -> F
and gCH _c _i =
  match gCG _c _i with F -> F | S (_, _i) ->
  expr _c _i
and gCG _c _i =
  if not (S.match_string "if" _c.s _i) then F else let _i = _i+2 in
  gCF _c _i
and gCF _c _i =
  if gCE _c _i <> F then F else ws _c _i
and gCE _c _i =
  match S.read_char _c.s _i with Some c when (c >= 'a' && c <= 'z') || ((c >= 'A' && c <= 'Z') || ((c >= '0' && c <= '9') || (c = '_'))) -> S ((), _i+1) | _ -> F
and gCD _c _i =
  match gCC _c _i with F -> gB5 _c _i | s -> s
and gCC _c _i =
  match gB_ _c _i with F -> F | S (r0, _i) ->
  match res_type _c _i with F -> F | S (r1, _i) ->
  match gCB _c _i with F -> F | S (r2, _i) ->
  S (Fun (r0, r1, r2), _i)
and gCB _c _i =
  match gCA _c _i with F -> F | S (_, _i) ->
  expr _c _i
and gCA _c _i =
  match gB' _c _i with F -> F | S (r1, _i) ->
  match ws _c _i with F -> F | S (_, _i) ->
  S (r1, _i)
and gB' _c _i =
  if S.match_string "->" _c.s _i then S ((), _i+2) else F
and gB_ _c _i =
  match gB9 _c _i with F -> F | S (_, _i) ->
  gB6 _c _i
and gB9 _c _i =
  if not (S.match_string "fun" _c.s _i) then F else let _i = _i+3 in
  gB8 _c _i
and gB8 _c _i =
  if gB7 _c _i <> F then F else ws _c _i
and gB7 _c _i =
  match S.read_char _c.s _i with Some c when (c >= 'a' && c <= 'z') || ((c >= 'A' && c <= 'Z') || ((c >= '0' && c <= '9') || (c = '_'))) -> S ((), _i+1) | _ -> F
and gB6 _c _i =
  let rec iter _i =
    match typed_arg _c _i with | F -> ([], _i) | S (r, _i) ->
    let (l, _i) = iter _i in (r :: l, _i)
  in
  let (l, _i) = iter _i in
  S (l, _i)
and gB5 _c _i =
  match gB4 _c _i with F -> gBr _c _i | s -> s
and gB4 _c _i =
  match gBv _c _i with F -> F | S (r0, _i) ->
  match gBw _c _i with F -> F | S (r1, _i) ->
  match res_type _c _i with F -> F | S (r2, _i) ->
  match gBz _c _i with F -> F | S (r3, _i) ->
  match gB3 _c _i with F -> F | S (r4, _i) ->
  S (Let (r0, r1, r2, r3, r4), _i)
and gB3 _c _i =
  match gB2 _c _i with F -> F | S (_, _i) ->
  expr _c _i
and gB2 _c _i =
  if not (S.match_string "in" _c.s _i) then F else let _i = _i+2 in
  gB1 _c _i
and gB1 _c _i =
  if gB0 _c _i <> F then F else ws _c _i
and gB0 _c _i =
  match S.read_char _c.s _i with Some c when (c >= 'a' && c <= 'z') || ((c >= 'A' && c <= 'Z') || ((c >= '0' && c <= '9') || (c = '_'))) -> S ((), _i+1) | _ -> F
and gBz _c _i =
  match gBy _c _i with F -> F | S (_, _i) ->
  expr _c _i
and gBy _c _i =
  match gBx _c _i with F -> F | S (r1, _i) ->
  match ws _c _i with F -> F | S (_, _i) ->
  S (r1, _i)
and gBx _c _i =
  if S.match_char '=' _c.s _i then S ((), _i+1) else F
and gBw _c _i =
  let rec iter _i =
    match typed_arg _c _i with | F -> ([], _i) | S (r, _i) ->
    let (l, _i) = iter _i in (r :: l, _i)
  in
  let (l, _i) = iter _i in
  S (l, _i)
and gBv _c _i =
  match gBu _c _i with F -> F | S (_, _i) ->
  id _c _i
and gBu _c _i =
  if not (S.match_string "let" _c.s _i) then F else let _i = _i+3 in
  gBt _c _i
and gBt _c _i =
  if gBs _c _i <> F then F else ws _c _i
and gBs _c _i =
  match S.read_char _c.s _i with Some c when (c >= 'a' && c <= 'z') || ((c >= 'A' && c <= 'Z') || ((c >= '0' && c <= '9') || (c = '_'))) -> S ((), _i+1) | _ -> F
and gBr _c _i =
  match gBq _c _i with F -> F | S (_, _i) ->
  gBo _c _i
and gBq _c _i =
  match gBp _c _i with F -> F | S (r1, _i) ->
  match ws _c _i with F -> F | S (_, _i) ->
  S (r1, _i)
and gBp _c _i =
  if S.match_char '(' _c.s _i then S ((), _i+1) else F
and gBo _c _i =
  match expr _c _i with F -> F | S (r1, _i) ->
  match gBn _c _i with F -> F | S (_, _i) ->
  S (r1, _i)
and gBn _c _i =
  match gBm _c _i with F -> F | S (r1, _i) ->
  match ws _c _i with F -> F | S (_, _i) ->
  S (r1, _i)
and gBm _c _i =
  if S.match_char ')' _c.s _i then S ((), _i+1) else F
and gBl _c _i =
  match gBk _c _i with F -> (mark _c _i "res_type"; F) | s -> s
and gBk _c _i =
  match gBj _c _i with F -> gBg _c _i | s -> s
and gBj _c _i =
  match gBi _c _i with F -> F | S (_, _i) ->
  type_ _c _i
and gBi _c _i =
  match gBh _c _i with F -> F | S (r1, _i) ->
  match ws _c _i with F -> F | S (_, _i) ->
  S (r1, _i)
and gBh _c _i =
  if S.match_char ':' _c.s _i then S ((), _i+1) else F
and gBg _c _i =
  S (TAny, _i)
and gBf _c _i =
  match gBe _c _i with F -> (mark _c _i "typed_arg"; F) | s -> s
and gBe _c _i =
  match gBd _c _i with F -> gBb _c _i | s -> s
and gBd _c _i =
  match id _c _i with F -> F | S (r0, _i) ->
  match gBc _c _i with F -> F | S (r1, _i) ->
  S ((r0, r1), _i)
and gBc _c _i =
  S (TAny, _i)
and gBb _c _i =
  match gBa _c _i with F -> F | S (_, _i) ->
  gBY _c _i
and gBa _c _i =
  match gBZ _c _i with F -> F | S (r1, _i) ->
  match ws _c _i with F -> F | S (_, _i) ->
  S (r1, _i)
and gBZ _c _i =
  if S.match_char '(' _c.s _i then S ((), _i+1) else F
and gBY _c _i =
  match gBX _c _i with F -> F | S (r1, _i) ->
  match gBT _c _i with F -> F | S (_, _i) ->
  S (r1, _i)
and gBX _c _i =
  match id _c _i with F -> F | S (r0, _i) ->
  match gBW _c _i with F -> F | S (r1, _i) ->
  S ((r0, r1), _i)
and gBW _c _i =
  match gBV _c _i with F -> F | S (_, _i) ->
  type_ _c _i
and gBV _c _i =
  match gBU _c _i with F -> F | S (r1, _i) ->
  match ws _c _i with F -> F | S (_, _i) ->
  S (r1, _i)
and gBU _c _i =
  if S.match_char ':' _c.s _i then S ((), _i+1) else F
and gBT _c _i =
  match gBS _c _i with F -> F | S (r1, _i) ->
  match ws _c _i with F -> F | S (_, _i) ->
  S (r1, _i)
and gBS _c _i =
  if S.match_char ')' _c.s _i then S ((), _i+1) else F
and gBR _c _i =
  match gBQ _c _i with F -> (mark _c _i "type"; F) | s -> s
and gBQ _c _i =
  match gBP _c _i with F -> gBL _c _i | s -> s
and gBP _c _i =
  match id _c _i with F -> F | S (r0, _i) ->
  match gBO _c _i with F -> F | S (r1, _i) ->
  S (TFun (r0, r1), _i)
and gBO _c _i =
  match gBN _c _i with F -> F | S (_, _i) ->
  type_ _c _i
and gBN _c _i =
  match gBM _c _i with F -> F | S (r1, _i) ->
  match ws _c _i with F -> F | S (_, _i) ->
  S (r1, _i)
and gBM _c _i =
  if S.match_string "->" _c.s _i then S ((), _i+2) else F
and gBL _c _i =
  match id _c _i with F -> F | S (r0, _i) ->
  S (TId (r0), _i)
and gBK _c _i =
  match gBJ _c _i with F -> gBG _c _i | s -> s
and gBJ _c _i =
  if not (S.match_string "true" _c.s _i) then F else let _i = _i+4 in
  gBI _c _i
and gBI _c _i =
  if gBH _c _i <> F then F else ws _c _i
and gBH _c _i =
  match S.read_char _c.s _i with Some c when (c >= 'a' && c <= 'z') || ((c >= 'A' && c <= 'Z') || ((c >= '0' && c <= '9') || (c = '_'))) -> S ((), _i+1) | _ -> F
and gBG _c _i =
  match gBF _c _i with F -> gBC _c _i | s -> s
and gBF _c _i =
  if not (S.match_string "false" _c.s _i) then F else let _i = _i+5 in
  gBE _c _i
and gBE _c _i =
  if gBD _c _i <> F then F else ws _c _i
and gBD _c _i =
  match S.read_char _c.s _i with Some c when (c >= 'a' && c <= 'z') || ((c >= 'A' && c <= 'Z') || ((c >= '0' && c <= '9') || (c = '_'))) -> S ((), _i+1) | _ -> F
and gBC _c _i =
  match gBB _c _i with F -> g_ _c _i | s -> s
and gBB _c _i =
  if not (S.match_string "let" _c.s _i) then F else let _i = _i+3 in
  gBA _c _i
and gBA _c _i =
  if g' _c _i <> F then F else ws _c _i
and g' _c _i =
  match S.read_char _c.s _i with Some c when (c >= 'a' && c <= 'z') || ((c >= 'A' && c <= 'Z') || ((c >= '0' && c <= '9') || (c = '_'))) -> S ((), _i+1) | _ -> F
and g_ _c _i =
  match g9 _c _i with F -> g6 _c _i | s -> s
and g9 _c _i =
  if not (S.match_string "in" _c.s _i) then F else let _i = _i+2 in
  g8 _c _i
and g8 _c _i =
  if g7 _c _i <> F then F else ws _c _i
and g7 _c _i =
  match S.read_char _c.s _i with Some c when (c >= 'a' && c <= 'z') || ((c >= 'A' && c <= 'Z') || ((c >= '0' && c <= '9') || (c = '_'))) -> S ((), _i+1) | _ -> F
and g6 _c _i =
  match g5 _c _i with F -> g2 _c _i | s -> s
and g5 _c _i =
  if not (S.match_string "fun" _c.s _i) then F else let _i = _i+3 in
  g4 _c _i
and g4 _c _i =
  if g3 _c _i <> F then F else ws _c _i
and g3 _c _i =
  match S.read_char _c.s _i with Some c when (c >= 'a' && c <= 'z') || ((c >= 'A' && c <= 'Z') || ((c >= '0' && c <= '9') || (c = '_'))) -> S ((), _i+1) | _ -> F
and g2 _c _i =
  match g1 _c _i with F -> gy _c _i | s -> s
and g1 _c _i =
  if not (S.match_string "if" _c.s _i) then F else let _i = _i+2 in
  g0 _c _i
and g0 _c _i =
  if gz _c _i <> F then F else ws _c _i
and gz _c _i =
  match S.read_char _c.s _i with Some c when (c >= 'a' && c <= 'z') || ((c >= 'A' && c <= 'Z') || ((c >= '0' && c <= '9') || (c = '_'))) -> S ((), _i+1) | _ -> F
and gy _c _i =
  match gx _c _i with F -> gu _c _i | s -> s
and gx _c _i =
  if not (S.match_string "then" _c.s _i) then F else let _i = _i+4 in
  gw _c _i
and gw _c _i =
  if gv _c _i <> F then F else ws _c _i
and gv _c _i =
  match S.read_char _c.s _i with Some c when (c >= 'a' && c <= 'z') || ((c >= 'A' && c <= 'Z') || ((c >= '0' && c <= '9') || (c = '_'))) -> S ((), _i+1) | _ -> F
and gu _c _i =
  match gt _c _i with F -> gq _c _i | s -> s
and gt _c _i =
  if not (S.match_string "else" _c.s _i) then F else let _i = _i+4 in
  gs _c _i
and gs _c _i =
  if gr _c _i <> F then F else ws _c _i
and gr _c _i =
  match S.read_char _c.s _i with Some c when (c >= 'a' && c <= 'z') || ((c >= 'A' && c <= 'Z') || ((c >= '0' && c <= '9') || (c = '_'))) -> S ((), _i+1) | _ -> F
and gq _c _i =
  if not (S.match_string "not" _c.s _i) then F else let _i = _i+3 in
  gp _c _i
and gp _c _i =
  if go _c _i <> F then F else ws _c _i
and go _c _i =
  match S.read_char _c.s _i with Some c when (c >= 'a' && c <= 'z') || ((c >= 'A' && c <= 'Z') || ((c >= '0' && c <= '9') || (c = '_'))) -> S ((), _i+1) | _ -> F
and gn _c _i =
  match gm _c _i with F -> (mark _c _i "lit_string"; F) | s -> s
and gm _c _i =
  if not (S.match_char '"' _c.s _i) then F else let _i = _i+1 in
  gl _c _i
and gl _c _i =
  match gk _c _i with F -> F | S (r1, _i) ->
  match ge _c _i with F -> F | S (_, _i) ->
  S (r1, _i)
and gk _c _i =
  match gj _c _i with F -> F | S (_, _i') ->
  S (S.sub _c.s _i ~len:(_i'-_i), _i')
and gj _c _i =
  let rec iter _i =
    match gi _c _i with | F -> ([], _i) | S (r, _i) ->
    let (l, _i) = iter _i in (r :: l, _i)
  in
  let (l, _i) = iter _i in
  S (l, _i)
and gi _c _i =
  match gh _c _i with F -> gf _c _i | s -> s
and gh _c _i =
  if not (S.match_char '\\' _c.s _i) then F else let _i = _i+1 in
  gg _c _i
and gg _c _i =
  if _i < S.length _c.s then S ((), _i+1) else F
and gf _c _i =
  match S.read_char _c.s _i with Some c when not (c = '"') -> S ((), _i+1) | _ -> F
and ge _c _i =
  match gd _c _i with F -> F | S (r1, _i) ->
  match ws _c _i with F -> F | S (_, _i) ->
  S (r1, _i)
and gd _c _i =
  if S.match_char '"' _c.s _i then S ((), _i+1) else F
and gc _c _i =
  match gb _c _i with F -> (mark _c _i "lit_int"; F) | s -> s
and gb _c _i =
  match ga _c _i with F -> F | S (r1, _i) ->
  match ws _c _i with F -> F | S (_, _i) ->
  S (r1, _i)
and ga _c _i =
  match gZ _c _i with F -> F | S (_, _i') ->
  S (S.sub _c.s _i ~len:(_i'-_i), _i')
and gZ _c _i =
  match gY _c _i with F -> F | S (_, _i) ->
  gW _c _i
and gY _c _i =
  match gX _c _i with F -> S (None, _i) | S (s, _i) -> S (Some s, _i)
and gX _c _i =
  if S.match_char '-' _c.s _i then S ((), _i+1) else F
and gW _c _i =
  match gV _c _i with F -> F | S (hd, _i) ->
  let rec iter _i =
    match gV _c _i with | F -> ([], _i) | S (r, _i) ->
    let (l, _i) = iter _i in (r :: l, _i)
  in
  let (l, _i) = iter _i in
  S (hd :: l, _i)
and gV _c _i =
  match S.read_char _c.s _i with Some c when c >= '0' && c <= '9' -> S ((), _i+1) | _ -> F
and gU _c _i =
  match gT _c _i with F -> (mark _c _i "id"; F) | s -> s
and gT _c _i =
  if any_key _c _i <> F then F else gS _c _i
and gS _c _i =
  match gR _c _i with F -> F | S (r1, _i) ->
  match ws _c _i with F -> F | S (_, _i) ->
  S (r1, _i)
and gR _c _i =
  match gQ _c _i with F -> F | S (_, _i') ->
  S (S.sub _c.s _i ~len:(_i'-_i), _i')
and gQ _c _i =
  match gP _c _i with F -> F | S (_, _i) ->
  gO _c _i
and gP _c _i =
  match S.read_char _c.s _i with Some c when (c >= 'a' && c <= 'z') || ((c >= 'A' && c <= 'Z') || (c = '_')) -> S ((), _i+1) | _ -> F
and gO _c _i =
  let rec iter _i =
    match gN _c _i with | F -> ([], _i) | S (r, _i) ->
    let (l, _i) = iter _i in (r :: l, _i)
  in
  let (l, _i) = iter _i in
  S (l, _i)
and gN _c _i =
  match S.read_char _c.s _i with Some c when (c >= 'a' && c <= 'z') || ((c >= 'A' && c <= 'Z') || ((c = '_') || (c >= '0' && c <= '9'))) -> S ((), _i+1) | _ -> F
and gM _c _i =
  let rec iter _i =
    match gL _c _i with | F -> ([], _i) | S (r, _i) ->
    let (l, _i) = iter _i in (r :: l, _i)
  in
  let (l, _i) = iter _i in
  S (l, _i)
and gL _c _i =
  match gK _c _i with F -> gI _c _i | s -> s
and gK _c _i =
  match comment _c _i with F -> F | S (_, _i) ->
  gJ _c _i
and gJ _c _i =
  S ([], _i)
and gI _c _i =
  match gH _c _i with F -> F | S (hd, _i) ->
  let rec iter _i =
    match gH _c _i with | F -> ([], _i) | S (r, _i) ->
    let (l, _i) = iter _i in (r :: l, _i)
  in
  let (l, _i) = iter _i in
  S (hd :: l, _i)
and gH _c _i =
  match S.read_char _c.s _i with Some c when (c = ' ') || ((c = '\t') || ((c = '\r') || (c = '\n'))) -> S ((), _i+1) | _ -> F
and gG _c _i =
  if not (S.match_string "(*" _c.s _i) then F else let _i = _i+2 in
  match gF _c _i with F -> F | S (_, _i) ->
  if S.match_string "*)" _c.s _i then S ((), _i+2) else F
and gF _c _i =
  let rec iter _i =
    match gE _c _i with | F -> ([], _i) | S (r, _i) ->
    let (l, _i) = iter _i in (r :: l, _i)
  in
  let (l, _i) = iter _i in
  S (l, _i)
and gE _c _i =
  match comment _c _i with F -> gD _c _i | s -> s
and gD _c _i =
  if gC _c _i <> F then F else gB _c _i
and gC _c _i =
  if S.match_string "*)" _c.s _i then S ((), _i+2) else F
and gB _c _i =
  if _i < S.length _c.s then S ((), _i+1) else F
and comment _c _i =
  gG _c _i
and ws _c _i =
  gM _c _i
and id _c _i =
  gU _c _i
and lit_int _c _i =
  gc _c _i
and lit_string _c _i =
  gn _c _i
and any_key _c _i =
  gBK _c _i
and type_ _c _i =
  gBR _c _i
and typed_arg _c _i =
  gBf _c _i
and res_type _c _i =
  gBl _c _i
and expr6 _c _i =
  gCj _c _i
and expr5 _c _i =
  gCk _c _i
and expr4 _c _i =
  gCv _c _i
and expr3 _c _i =
  gCw _c _i
and expr2 _c _i =
  gCx _c _i
and expr1 _c _i =
  gCy _c _i
and expr0 _c _i =
  gCz _c _i
and expr _c _i =
  expr0 _c _i
and decl _c _i =
  gDA _c _i
and program _c _i =
  gDE _c _i
module SSet = Set.Make(String)
let unique l = SSet.elements (SSet.of_list l)
let program _s _i =
  let _c = { s = _s; maxidx = 0; tags = [ "program" ] } in
  match program _c _i with
    | S (ast, _i) -> Ok (ast, _i)
    | F -> Error (unique _c.tags, _c.maxidx)
