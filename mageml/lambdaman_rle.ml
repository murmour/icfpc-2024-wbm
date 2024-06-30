let loop a =
  (fun b -> a (b b))(fun b -> a (b b));

let map_char (i: int): string =
    take 1 (drop i "URDL");


let copy_str (s: string) (num: int) : string =
    let copy_loop = loop(fun copy (s: string) (n: int) : string ->
        if n = 1 then s
        else (copy s (n - 1)) ^ s
    ) in
    copy_loop s num;

let rle_decode (block: int) (count_p: int) (char_p: int) : string =
    let loop1 = loop(fun dec (b: int) (c_p: int) (ch_p: int) : string ->
        let c = b % ch_p in
        let b1 = b / ch_p in
        let n = b1 % c_p in
        let b2 = b1 / c_p in
        let tmp = copy_str (map_char (trace c)) (trace n) in

        if b2 > 0 then
            (dec b2 c_p ch_p) ^ tmp
        else
            tmp
    ) in
    loop1 block count_p char_p;


let magic = "solve lambdaman4 " ^ (rle_decode 52647 4 4);
