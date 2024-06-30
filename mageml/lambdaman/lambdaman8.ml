(* score: 136 STRIP B$ L! v! BEFORE SEND! *)

let magic =
  "solve lambdaman8 " ^
  (fun f -> f ((f "DD") ^ (f "LL") ^ (f "UU") ^ (f "RR")))
  (fun y -> (fun x -> x (x (x (x y)))) (fun x -> x ^ x ^ x))
;
