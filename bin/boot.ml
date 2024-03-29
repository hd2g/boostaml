open Core

let () = Sys.getcwd () |> print_endline

let getenv_opt = Dotenv.config ()

let token =
  getenv_opt "BOOST_NOTE_TOKEN"
  |> (function
      | Some x -> x
      | None -> failwith "BOOST_NOTE_TOKEN isn't defined")

let list_documents =
  Command.basic
    ~summary:"List documents"
    (let open Command.Let_syntax in
     let%map_open title = flag "--title" (optional string) ~doc:"title filter by title"
     and archived = flag "--archived" no_arg ~doc:"filter by archived"
     and workspace_id = flag "--workspace-id" (optional string) ~doc:"id fitler by workspace id"
     and parent_folder_id = flag "--parent-folder-id" (optional string) ~doc:"id filter by parent folder id"
     and order_by = flag "--order-by" (optional string) ~doc:"key filter by order by (created-at|updated-at)"
     in
     (fun () ->
       let instance = Boostnote.Instance.(make ~base_url ~token) in
       Boostnote.Documents.list instance ?title ~archived ?workspace_id ?parent_folder_id ?order_by ()
       |> Lwt_main.run
       |> (function
           | Ok body -> print_endline body
           | Error message -> message |> print_endline)))

let get_document =
  Command.basic
    ~summary:"Get a document"
    (let open Command.Let_syntax in
     let%map_open document_id = anon ("document_id" %: string)
     in
     (fun () ->
       let instance = Boostnote.Instance.(make ~base_url ~token) in
       Boostnote.Documents.get instance ~document_id ()
       |> Lwt_main.run
       |> (function
           | Ok body -> print_endline body
           | Error message -> message |> print_endline)))

let create_docuemnt =
  Command.basic
    ~summary:"Create a document"
    (let open Command.Let_syntax in
     let%map_open title = anon ("title" %: string)
     and content = anon ("content" %: string)
     and workspace_id = flag "--workspace-id" (optional string) ~doc:"id target workspace id"
     and parent_folder_id = flag "--parnet-folder-id" (optional string) ~doc:"id target parent folder id"
     and tags = flag "--tags" (optional string) ~doc:"tags tags to add"
     in
     (fun () ->
       let instance = Boostnote.Instance.(make ~base_url ~token) in
       Boostnote.Documents.create
         instance
         ~title ~content
         ?workspace_id ?parent_folder_id ?tags
         ()
       |> Lwt_main.run
       |> (function
           | Ok body -> body |> print_endline
           | Error message -> message |> print_endline)))

let update_document =
  Command.basic
    ~summary:"Update a dococument"
    (let open Command.Let_syntax in
     let%map_open document_id = anon ("document-id" %: string)
     and title = flag "--title" (optional string) ~doc:"title New title"
     and content = flag "--content" (optional string) ~doc:"content New content"
     and emoji = flag "--emoji" (optional string) ~doc:"emoji New emoji"
     and workspace_id = flag "--workspace-id" (optional string) ~doc:"id Target workspace id"
     and parent_folder_id = flag "--parent-folder-id" (optional string) ~doc:"id Target parent folder id"
     in
     (fun () ->
       let instance = Boostnote.Instance.(make ~base_url ~token) in
       Boostnote.Documents.update
         instance
         ~document_id
         ?title ?content ?emoji ?workspace_id ?parent_folder_id
         ()
       |> Lwt_main.run
       |> (function
           | Ok body -> body |> print_endline
           | Error message -> message |> print_endline)))

let delete_document =
  Command.basic
    ~summary:"Delete a document"
    (let open Command.Let_syntax in
     let%map_open document_id = anon ("document-id" %: string) in
     (fun () ->
       let instance = Boostnote.Instance.(make ~base_url ~token) in
       Boostnote.Documents.delete instance ~document_id ()
       |> Lwt_main.run
       |> (function
           | Ok body -> body |> print_endline
           | Error message -> message |> print_endline)))

let documents =
  Command.group
    ~summary:"Commands about documents"
    [ "list", list_documents
    ; "get", get_document
    ; "create", create_docuemnt
    ; "update", update_document
    ; "delete", delete_document
    ]

(* let list_folders = *)
(*   Command.basic *)
(*     ~summary:"List folder" *)
(*     (let open Command.Let_syntax in *)
(*      let%map_open  *)

let list_folders =
  Command.basic
    ~summary:"List folders"
    (let open Command.Let_syntax in
     let%map_open name = flag "--name" (optional string) ~doc:"name filter by name"
     and workspace_id = flag "--workspace-id" (optional string) ~doc:"id filter by workspace id"
     and parent_folder_id = flag "--parent-folder-id" (optional string) ~doc:"id filter by parent folder id"
     and order_by = flag "--order-by" (optional string) ~doc:"(CreatedAt|UpdatedAt) order by"
     in
     (fun () ->
       let instance = Boostnote.Instance.(make ~base_url ~token) in
       Boostnote.Folders.list ?name ?workspace_id ?parent_folder_id ?order_by instance ()
       |> Lwt_main.run
       |> (function
           | Ok body -> body |> print_endline
           | Error message -> message |> print_endline)))

let get_folder =
  Command.basic
    ~summary:"Get a folder"
    (let open Command.Let_syntax in
     let%map_open folder_id = anon ("folder-id" %: string) in
     (fun () ->
       let instance = Boostnote.Instance.(make ~base_url ~token) in
       Boostnote.Folders.get ~folder_id instance ()
       |> Lwt_main.run
       |> (function
           | Ok body -> body |> print_endline
           | Error message -> message |> print_endline)))

let create_folder =
  Command.basic
    ~summary:"Create a folder"
    (let open Command.Let_syntax in
     let%map_open name = anon ("name" %: string)
     and emoji = flag "--emoji" (optional string) ~doc:"emoji Emoji for the folder to be created"
     and workspace_id = flag "--workspace-id" (optional string) ~doc:"id Workspace in which to create folder"
     and parent_folder_id = flag "--parent-folder-id" (optional string) ~doc:"Folder in which to create folder. If passd with workspaceId folder must be in that workspace"
     in
     (fun () ->
       let instance = Boostnote.Instance.(make ~base_url ~token) in
       Boostnote.Folders.create ?emoji ?workspace_id ?parent_folder_id ~name instance ()
       |> Lwt_main.run
       |> (function
           | Ok body -> body |> print_endline
           | Error message -> message |> print_endline)))

let delete_folder =
  Command.basic
    ~summary:"Delete a folder"
    (let open Command.Let_syntax in
     let%map_open folder_id = anon ("folder-id" %: string)
     and force = flag "--force" (optional string) ~doc:"(true|false) If passed and is 'true', it will delete the folder alogn with all it's contents. (This is not recoverable)"
     in
     (fun () ->
       let instance = Boostnote.Instance.(make ~base_url ~token) in
       Boostnote.Folders.delete ?force ~folder_id instance ()
       |> Lwt_main.run
       |> (function
           | Ok body -> body |> print_endline
           | Error message -> message |> print_endline)))

let folders =
  Command.group
    ~summary:"Commands about folders"
    [ "list", list_folders
    ; "get", get_folder
    ; "create", create_folder
    ; "delete", delete_folder
    ]

let command =
  Command.group
    ~summary:"Boost Note command line tool"
    [ "documents", documents
    ; "folders", folders
    ]

let () =
  Command.run
    ~version:"0.0.1"
    command
