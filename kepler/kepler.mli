
type expr
type eval_res


val parse_expr: string -> (expr, string) result
val print_expr: expr -> string
val eval: expr -> (eval_res, string) result
val print_res: eval_res -> string
