
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
  | App of expr list
  | Fun of typed_arg list * type_ * expr
  | Let of [`t|`f] * var * typed_arg list * type_ * expr * expr
  | Var of var


val program:
  CharStream.t ->
  CharStream.index ->
  (expr * int, string list * int) result
