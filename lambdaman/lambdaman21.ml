(*
auto bigu = mul(200, "U");
	auto bigl = mul(200, "L");
	auto bigr = mul(200, "R");
	auto bigd = mul(200, "D");

	string ru_walker = mul(200, bigr + bigl + 'D');
	string lu_walker = mul(200, bigl + bigr + 'D');

	string ud_walker = mul(80, bigu + bigd + 'L');

	string special = mul(10, "LLLLLLD" + bigl + bigr);

	auto path = bigu + take(61 * 401 + 40, ru_walker) + bigu + take((132 - 57) * 401 + 80, ru_walker) + special + bigu + ru_walker;
	path += take(89, bigr) + take(152, bigu) + lu_walker;
	path += bigu + lu_walker + take(104, bigu) + take(26, bigl) + ud_walker;
   *)

   
   let dup s = s ^ s ^ s in
   let qup s = dup (dup (dup (dup (dup s)))) in
   let bigl = qup "L" in
   let bigr = qup "R" in
   let bigu = qup "U" in
   let bigd = qup "D" in
   let ru_walker = qup (bigr ^ bigl ^ "D") in
   let lu_walker = qup (bigl ^ bigr ^ "D") in
   let ud_walker = qup (bigu ^ bigd ^ "L") in
   let special = qup ("LLLLLLD" ^ bigl ^ bigr) in
   "solve lambdaman21 " ^ bigu ^ (take 29747 ru_walker) ^ bigu ^ take 36605 ru_walker ^ special ^ bigu ^ ru_walker ^ take 89 bigr ^ take 152 bigu ^ lu_walker ^ bigu ^ lu_walker ^ take 104 bigu ^ take 26 bigl ^ ud_walker
  

(*    let dup s = s ^ s ^ s in
   let qup s = dup (dup (dup (dup (dup s)))) in
   qup "L" *)