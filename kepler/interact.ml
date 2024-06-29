
module BF = Buffer
module S = String

let sprintf = Printf.sprintf


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


let () =
  let response = send (Sys.argv.(1)) in
  Printf.printf "response:\n\n%s\n\n\n" response;
  match Kepler.parse_expr response with
    | Error msg ->
        Printf.eprintf "response parse error: %s\n" msg;
        exit 1
    | Ok e ->
        let pretty = Kepler.print_expr e in
        Printf.printf "pretty:\n\n%s\n\n\n" pretty;
        match Kepler.eval e with
          | Error msg ->
              Printf.eprintf "eval error: %s\n" msg;
              exit 1
          | Ok e ->
              Printf.printf "eval:\n\n%s\n" (Kepler.print_res e)
