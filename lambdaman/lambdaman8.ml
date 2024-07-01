(* score: 136 *)

let dup x = x ^ x ^ x in
let qup x = dup (dup (dup (dup x))) in
"solve lambdaman8 " ^
qup ((qup "DD") ^ (qup "LL") ^ (qup "UU") ^ (qup "RR"))
