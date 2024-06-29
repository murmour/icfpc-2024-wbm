
type t = {
  len: int;
  chars: string;
  lines: (int array) Lazy.t; (* sorted line indices *)
}

type index = int

type pos = {
  line: int;
  column: int;
}


let uget = String.unsafe_get


module DynArray = struct

  type 'a t = {
    mutable arr: 'a array;
    mutable len: int;
  }


  (* A polymorphic placeholder that is never accessed by the user *)
  let dummy_value =
    Obj.magic 0

  let create ?(size = 10) () =
    {
      len = 0;
      arr = Array.make size dummy_value;
    }

  let grow darr =
    match Array.length darr.arr with
      | 0 ->
          darr.arr <- Array.make 16 dummy_value
      | n ->
          let new_arr = Array.make (n*2) dummy_value in
          Array.blit darr.arr 0 new_arr 0 n;
          darr.arr <- new_arr

  let add darr v =
    if darr.len = Array.length darr.arr then
      grow darr;
    Array.unsafe_set darr.arr darr.len v;
    darr.len <- darr.len + 1

  let get darr idx =
    if idx < 0 || idx >= darr.len then
      invalid_arg "MParser_Utils.DynArray.get: invalid index";
    Array.unsafe_get darr.arr idx

  let set darr idx v =
    if idx < 0 || idx >= darr.len then
      invalid_arg "MParser_Utils.DynArray.set: invalid index";
    Array.unsafe_set darr.arr idx v

  let length darr =
    darr.len

  let to_array darr =
    Array.sub darr.arr 0 darr.len

end


let find_lines (str: string) : int array =
  let len = String.length str in
  let lines = DynArray.create ~size:(len/10) () in
  DynArray.add lines 0;
  let rec scan i =
    if i = len-1 then
      (if let c = uget str i in c = '\r' || c = '\n' then
         DynArray.add lines (i+1))
    else if i < len-1 then
      match (uget str i, uget str (i+1)) with
        | ('\r', '\n') ->
            DynArray.add lines (i+2);
            scan (i+2)
        | (('\r' | '\n'), _) ->
            DynArray.add lines (i+1);
            scan (i+1)
        | _ ->
            scan (i+1)
  in
  scan 0;
  DynArray.to_array lines

let from_string str : t =
  {
    len = String.length str;
    chars = str;
    lines = lazy (find_lines str);
  }

let is_valid_idx s i =
  i >= 0 && i < s.len

let length s =
  s.len

let read_char s i =
  if is_valid_idx s i then
    Some (uget s.chars i)
  else
    None

let match_char c s i =
  is_valid_idx s i && (uget s.chars i) = c

let match_string str s i =
  let len = String.length str in
  if i < 0 || i + len > s.len then
    false
  else
    let rec iter j =
      if j = len then
        true
      else if uget s.chars (i+j) = uget str j then
        iter (j+1)
      else
        false
    in
    iter 0

let skip_newline s i =
  if not (is_valid_idx s i) then
    None
  else match uget s.chars i with
    | '\r' when i+1 < s.len && uget s.chars (i+1) = '\n' ->
        Some (i+2)
    | '\r' | '\n' ->
        Some (i+1)
    | _ ->
        None

let is_bol s i =
  if i = 0 then
    true
  else if not (is_valid_idx s i) then
    false
  else match (uget s.chars (i-1), uget s.chars i) with
    | ('\r', '\n') ->
        false
    | (('\r' | '\n'), _) ->
        true
    | _ ->
        false

let is_eol s i =
  if i < 0 then
    false
  else if i >= s.len then
    true
  else if i = 0 then
    let c = uget s.chars i in c = '\r' || c = '\n'
  else match (uget s.chars (i-1), uget s.chars i) with
    | ('\r', '\n') ->
        false
    | (_, ('\r' | '\n')) ->
        true
    | _ ->
        false

let sub s i ~len =
  if not (i >= 0 && len >= 0 && i+len <= s.len) then
    invalid_arg "MParser_Stream.sub: invalid arguments";
  String.sub s.chars i len


(* Handling positions
   -------------------------------------------------------------------------- *)

let find_line_idx (lines: int array) i : int =
  (* Binary search over an array of sorted line indices *)
  let rec iter low high : int =
    let line_begin = Array.unsafe_get lines high in
    if i >= line_begin then
      high
    else
      let pivot = (high + low) / 2 in
      if i < Array.unsafe_get lines pivot then
        iter low (pivot-1)
      else
        iter pivot (high-1)
  in
  iter 0 (Array.length lines - 1)

let get_pos (s: t) i : pos =
  if s.len = 0 || i < 0 then
    { line = 0; column = 0 }
  else
    let lines = Lazy.force s.lines in
    let line_idx = find_line_idx lines i in
    let line_start = Array.unsafe_get lines line_idx in
    { line = line_idx + 1; column = i - line_start + 1 }

let print_info ~width ~indent (s: t) i : string =
  if not (width >= 0 && indent >= 0 && i >= 0) then
    invalid_arg "MParser_Stream.print_info: invalid arguments";
  let space = width - indent in
  if space <= 10 then
    "\n"
  else
    let rec find_stop i len : int =
      if len = 0 || is_eol s i then i else find_stop (i+1) (len-1)
    in
    let pos = get_pos s i in
    let start = i - (min pos.column (space / 2)) in
    let stop = find_stop start space in
    let len = stop - start in
    if len <= 0 then
      "\n"
    else
      let offset = i - start in
      let ind = String.make indent ' ' in
      ind ^ (Printf.sprintf "Line %d, Column %d:\n" pos.line pos.column)
      ^ ind ^ (sub s start ~len) ^ "\n"
      ^ ind ^ (String.make offset ' ') ^ "^\n"
