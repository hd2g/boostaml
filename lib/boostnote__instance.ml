type t =
  { base_url : string
  ; token : string
  }

let base_url = "https://boostnote.io/api"

let verify ~base_url ~token =
  base_url <> "" && token <> ""

let make ~base_url ~token =
  assert (verify ~base_url ~token);
  { base_url; token }

let headers_of { token; _ } =
  [ "Content-Type", "application/json"
  ; "Authorization", Printf.sprintf "Bearer %s" token ]

let url_of rest { base_url; _ } =
  Printf.sprintf "%s%s" base_url rest
