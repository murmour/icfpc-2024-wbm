let loop a =
  (fun b -> a (b b))(fun b -> a (b b)) 
in
let ff = loop (fun ff t -> 
    if t = 0 then
      "solve lambdaman4 " 
    else
      ff (t / 4) ^ take 1 (drop (t % 4) "RDUL")
)
in 
  ff 42424242