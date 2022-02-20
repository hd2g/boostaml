module Method = struct
  type t =
    | Get
    | Post
    | Put
    | Delete
    | Patch
    | Options
    | Connect

  let of_string s =
    s
    |> String.lowercase_ascii
    |> (function
        | "get" -> Get
        | "post" -> Post
        | "put" -> Put
        | "delete" -> Delete
        | "patch" -> Patch
        | "options" -> Options
        | "connect" -> Connect
        | _ -> raise @@ Invalid_argument s)

  let string_of = function
    | Get -> "GET"
    | Post -> "POST"
    | Put -> "PUT"
    | Delete -> "DELETE"
    | Patch -> "PATCH"
    | Options -> "OPTIONS"
    | Connect -> "CONNECT"
end

module type Client = sig
  type response

  val request :
    ?queries:(string * string) list ->
    ?body:(string * string) list ->
    ?headers:(string * string) list ->
    meth:Method.t ->
    string ->                   (* url *)
    response
end

module Implement (Client : Client) = struct
  type response = Client.response
  let request = Client.request

  let get = request ~meth:Method.Get
  let post = request ~meth:Method.Post
  let put = request ~meth:Method.Put
  let delete = request ~meth:Method.Delete
  let patch = request ~meth:Method.Patch
  let options = request ~meth:Method.Options
  let connect = request ~meth:Method.Connect
end

include Implement(struct
  type response = (string, Piaf.Error.t) Lwt_result.t

  let piaf_method_of_meth (meth : Method.t) : Piaf.Method.t =
    Method.string_of meth |> Piaf.Method.of_string

  let piaf_body_of_body body : Piaf.Body.t option =
    Base.Option.try_with (fun () ->
        let body' =
          List.map
            (fun (k, v) -> (k, `Stringlit (Printf.sprintf {|"%s"|} v)))
            body
        in
        `Assoc body'
        |> Yojson.Raw.to_string
        |> (fun json -> print_endline ("body: " ^ json); json)
        |> Piaf.Body.of_string)

  let request ?(queries = []) ?(body = []) ?(headers = []) ~meth url =
    let uri =
      if ((List.length queries) = 0) then
        Uri.of_string url
      else
        Uri.(add_query_params' (of_string url) queries)
    in
    let body =
      if (meth = Method.Get || List.length body = 0) then
        None
      else
        match (piaf_body_of_body body) with
        | Some _  as body ->  body
        | None -> failwith @@ "Parse body as json failed"
    in
    let meth = piaf_method_of_meth meth in
    let open Lwt_result.Syntax in
    let* res = Piaf.Client.Oneshot.request ~headers ?body ~meth uri in
    if (Piaf.Status.is_successful res.status) then
      Piaf.Body.to_string res.body
    else
      let message = Piaf.Status.to_string res.status in
      Lwt.return (Error (`Msg message))
end)
