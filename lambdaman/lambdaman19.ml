(*
string rec(int len, int t) {
		if (len == 0) return "";
		char c = "DRUL"[t];
		char rev = "DRUL"[(t + 2) % 4];
		return string(len, c) + rec(len / 2, (t + 3) % 4) + rec(len / 2, t) + rec(len / 2, (t + 1) % 4) + string(len, rev);
	}

	void solve19() {
		string path = "";
		for (int i = 0; i < 4; i++)
			path = path + rec(64, i);
		run_path(path);
	}
   *)
   let dup s = s ^ s ^ s in
   let loop a =
     (fun b -> a (b b))(fun b -> a (b b)) 
   in
   let ff = loop (fun ff len t -> 
     let len2 = len / 2 in
       if len = 0 then 
         "" 
       else
         let c = take 1 (drop t "DRUL") in
         let r = take 1 (drop ((t + 2) % 4) "DRUL") in
         let cc = take len (dup (dup (dup (dup c)))) in
         let rr = take len (dup (dup (dup (dup r)))) in
         cc ^ ff len2 ((t+3) % 4) ^ ff len2 t ^ ff len2 ((t+1) % 4) ^ rr)
   in
   "solve lambdaman19 " ^ ff 64 0 ^ ff 64 1 ^ ff 64 2 ^ ff 64 3