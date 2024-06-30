let loop a =
  (fun b -> a (b b))(fun b -> a (b b));

(*let map_char (i: int): string =
    take 1 (drop i "URDL");*)

(*let rle_decode (block: int) (count_p: int) (char_p: int) : string =
    let loop1 = loop(fun dec (b: int) (c_p: int) (ch_p: int) : string ->
        let copy_loop = loop(fun copy (s: string) (n: int) : string ->
            if n = 1 then s
            else (copy s (n - 1)) ^ s
        ) in
        if b > 0 then
            (dec ((b / ch_p) / c_p) c_p ch_p) ^ (copy_loop (map_char (b % ch_p)) ((b / ch_p) % c_p))
        else
            ""
    ) in
    loop1 block count_p char_p;*)

(*let rle_decode (block: int) : string =
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
    loop1 block;*)

(* let magic = "solve lambdaman4 " ^ (rle_decode 52647); *)

let magic =
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
    "solve lambdaman4 " ^ (loop1 52647);
