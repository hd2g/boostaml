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
