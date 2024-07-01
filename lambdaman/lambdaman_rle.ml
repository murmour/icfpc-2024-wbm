let loop a =
  (fun b -> a (b b))(fun b -> a (b b)) 
in
let loop1 = loop(fun dec (b: int) : string ->
    let copy_loop = loop(fun copy (s: string) (n: int) : string ->
        if n = 1 then s
        else (copy s (n - 1)) ^ s
    ) in
    if b > 0 then
        (dec (b / 16)) ^ (copy_loop (take 1 (drop (b % 4) "URDL")) ((b / 4) % 4))
    else
        ""
) in
"solve lambdaman6 " ^ (loop1 52647)
