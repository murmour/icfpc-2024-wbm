(*
int t_left[] = {1, 0, 3, 2};

string rec(int level, int t) {
	if (level == 0) return "";
	char c = "DRUL"[t];
	return rec(level - 1, t_left[t]) + rec(level - 1, t) + c + rec(level - 1, t) + rec(level - 1, 3 - t);
}   
*)
let dup s = s ^ s ^ s in
let loop a =
  (fun b -> a (b b))(fun b -> a (b b)) 
in
let ff = loop (fun ff level t -> 
  let lev = level - 1 in
    if level = 0 then 
      "" 
    else
      let a = ff lev t in
      let c = take 1 (drop t "DRUL") in
      ff lev (if t = 0 then 1 else if t = 1 then 0 else if t = 2 then 3 else 2) ^ a ^ dup (dup c) ^ a ^ ff lev (3 - t)) 
in
"solve lambdaman16 " ^ ff 7 1