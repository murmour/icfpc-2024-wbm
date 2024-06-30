
module BF = Buffer
module S = String

let printf = Printf.printf
let sprintf = Printf.sprintf
let eprintf = Printf.eprintf


let send_url = "https://boundvariable.space/communicate"
let auth_header = "Authorization: Bearer 00814e29-be7a-4b5a-ad13-ee45f0c5c37c"

let curl_initialized = ref false

let http_post ~url ~data : int * string =
  if not !curl_initialized then (
    Curl.global_init Curl.CURLINIT_GLOBALALL;
    curl_initialized := true
  );
  let b = BF.create 100 in
  let c = Curl.init () in
  Curl.set_httpheader c [ auth_header ];
  Curl.set_timeout c 100; (* seconds *)
  Curl.set_sslverifypeer c false;
  Curl.set_sslverifyhost c Curl.SSLVERIFYHOST_EXISTENCE;
  Curl.set_writefunction c (fun s -> BF.add_string b s; S.length s);
  Curl.set_tcpnodelay c true;
  Curl.set_verbose c false;
  Curl.set_url c url;
  Curl.set_post c true;
  Curl.set_postfields c data;
  Curl.set_postfieldsize c (S.length data);
  Curl.perform c;
  let code = Curl.get_responsecode c in
  Curl.cleanup c;
  (code, BF.contents b)


let send (data: string) : string =
  match http_post ~url:send_url ~data with
    | (200, body) ->
        body
    | (code, body) ->
        failwith (sprintf "Unexpected HTTP response (%d): %s" code body)


let send_pretty (request: string) : unit =
  let response = send request in
  printf "response:\n\n%s\n\n\n" response;
  match Kepler.parse_expr response with
    | Error msg ->
        eprintf "response parse error: %s\n" msg;
        exit 1
    | Ok e ->
        let pretty = Kepler.print_expr e in
        printf "pretty:\n\n%s\n\n\n" pretty;
        match Kepler.eval e with
          | Error msg ->
              eprintf "eval error: %s\n" msg;
              exit 1
          | Ok e ->
              printf "eval:\n\n%s\n" (Kepler.print_res e)


let send_string_to_string (request: string) : unit =
  let request = sprintf "S%s" (Kepler.encode_string request) in
  let response = send request in
  match Kepler.parse_expr response with
    | Error msg ->
        eprintf "response parse error: %s\n" msg;
        exit 1
    | Ok e ->
        match Kepler.eval e with
          | Error msg ->
              eprintf "eval error: %s\n" msg;
              exit 1
          | Ok e ->
              output_string stdout (Kepler.print_raw_res e)


let read_all ch : string =
  let buf_size = 4096 in
  let buf = BF.create buf_size in
  let bytes = Bytes.create buf_size in
  let rec iter () =
    let len = input ch bytes 0 buf_size in
    if len > 0 then
      (BF.add_subbytes buf bytes 0 len;
       iter ())
  in
  iter ();
  BF.contents buf


let () =
  let input =
    match Sys.argv.(2) with
      | "stdin" -> read_all stdin
      | etc -> etc
  in
  match Sys.argv.(1) with
    | "-i" ->
        send_pretty input
    | "-s" ->
        send_pretty (sprintf "S%s" (Kepler.encode_string input))
    | "-ss" ->
        send_string_to_string input
    | etc ->
        eprintf "invalid arg: %s\n" etc;
        exit 1
