let loop a =
  (fun b -> a (b b))(fun b -> a (b b));

let copy_loop = loop(fun copy (s: string) (n: int) : string ->
        if n = 1 then s
        else (copy s (n - 1)) ^ s
    );

let magic = "solve lambdaman6 " ^ copy_loop "R" 199;
