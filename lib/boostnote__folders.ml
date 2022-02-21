type folder =
  { id : string
  ; emoji : string
  ; name : string
  ; pathname : string
  ; description : string
  ; parent_folder_id : string option
  ; team_id : string
  ; generated : bool
  ; created_at : string
  ; updated_at : string
  ; pageOrder : string
  ; version : int
  ; workspace_id : string
  ; child_docs_ids : string list
  ; child_folder_ids : string list
  }

include Boostnote__helpers

let list ?name ?workspace_id ?parent_folder_id ?order_by instance () =
  let uri = instance |> Boostnote__instance.url_of "/folders" in
  let headers = Boostnote__instance.headers_of instance in
  let queries =
    [ "name", name
    ; "workspaceId", workspace_id
    ; "parentFolderId", parent_folder_id
    ; "order_by", order_by
    ] |> body_of_list
  in
  Request.get ~headers ~queries uri

let get ~folder_id instance () =
  let uri = instance |> Boostnote__instance.url_of (Printf.sprintf "/folders/%s" folder_id) in
  let headers = Boostnote__instance.headers_of instance in
  Request.get ~headers uri

let create ?emoji ?workspace_id ?parent_folder_id ~name instance () =
  assert (match workspace_id, parent_folder_id with None, None -> false | _ -> true);
  let uri = instance |> Boostnote__instance.url_of "/folders" in
  let headers = Boostnote__instance.headers_of instance in
  let body =
    [ "name", Some name
    ; "emoji", emoji
    ; "workspaceId", workspace_id
    ; "parentFolderId", parent_folder_id
    ] |> body_of_list
  in
  Request.post ~headers ~body uri

let delete ?force ~folder_id instance () =
  let uri = instance |> Boostnote__instance.url_of (Printf.sprintf "/folders/%s" folder_id) in
  let headers = Boostnote__instance.headers_of instance in
  let force = Option.map process_bool_string_exn force in
  let body =
    [ "force", force
    ] |> body_of_list
  in
  Request.delete ~headers ~body uri
