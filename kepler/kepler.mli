
type expr
type eval_res


val encode_string: string -> string
val parse_expr: string -> (expr, string) result
val print_expr: expr -> string
val eval: expr -> (eval_res, string) result
val print_res: eval_res -> string
val print_raw_res: eval_res -> string
