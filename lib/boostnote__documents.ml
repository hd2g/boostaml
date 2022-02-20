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

let list ?title ?archived ?workspace_id ?parent_folder_id ?order_by instance () =
  let uri =
    instance
    |> Boostnote__instance.url_of "/docs"
  in
  let headers = Boostnote__instance.headers_of instance in
  Request.get ~headers uri

let get instance ~document_id () =
  let uri =
    instance
    |> Boostnote__instance.url_of
         (Printf.sprintf "/docs/%s" document_id)
  in
  let headers = Boostnote__instance.headers_of instance in
  Request.get ~headers uri

let create instance ~title ~content () = assert false
let update instance ~docid () = assert false
let delete instance ~docid () = assert false
