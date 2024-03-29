type doc =
  { id : string
  ; emoji : string option
  ; head : head
  ; workspace_id : string
  ; parent_folder_id : string
  ; archived_at : string
  ; created_at : string
  ; updated_at : string
  }

and head =
  { id : int
  ; content : string
  ; message : string
  ; created : string
  ; creators : creator list
  }

and creator =
  { id : string
  ; unique_name : string
  ; display_name : string
  ; created_at : string
  ; updated_at : string
  }

include Boostnote__helpers

let list ?title ?(archived = false) ?workspace_id ?parent_folder_id ?order_by instance () =
  let uri = instance |> Boostnote__instance.url_of "/docs" in
  let headers = Boostnote__instance.headers_of instance in
  let archived = archived |> Bool.to_string |> Option.some in
  let queries =
    [ "title", title
    ; "archived", archived
    ; "workspaceId", workspace_id
    ; "parentFolderId", parent_folder_id
    ; "orderBy", order_by
    ] |> body_of_list
  in
  Request.get ~headers ~queries uri

let get instance ~document_id () =
  let uri =
    instance
    |> Boostnote__instance.url_of
         (Printf.sprintf "/docs/%s" document_id)
  in
  let headers = Boostnote__instance.headers_of instance in
  Request.get ~headers uri

let create instance ~title ~content ?workspace_id ?parent_folder_id ?tags () =
  let uri = instance |> Boostnote__instance.url_of "/docs" in
  let headers = Boostnote__instance.headers_of instance in
  let body =
    [ "title", Some title
    ; "content", Some content
    ; "workspaceId", workspace_id
    ; "parentFolderId", workspace_id
    ; "tags", Option.map (Printf.sprintf "[%s]") tags
    ]
    |> body_of_list
  in
  Request.post ~headers ~body uri

let update
      ?title ?emoji ?content ?workspace_id ?parent_folder_id
      ~document_id
      instance () =
  let uri = instance |> Boostnote__instance.url_of (Printf.sprintf "/docs/%s" document_id) in
  let headers = Boostnote__instance.headers_of instance in
  let body =
    [ "title", title
    ; "content", content
    ; "emoji", emoji
    ; "workspaceId", workspace_id
    ; "parentFolderId", parent_folder_id
    ] |> body_of_list
  in
  Request.patch ~headers ~body uri

let delete instance ~document_id () =
  let uri = instance |> Boostnote__instance.url_of (Printf.sprintf "/docs/%s" document_id) in
  let headers = instance |> Boostnote__instance.headers_of in
  Request.delete ~headers uri
