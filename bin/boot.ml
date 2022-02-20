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
     let%map_open title = flag "--title" (optional string) ~doc:"filter by title"
     and archived = flag "--archived" no_arg ~doc:"filter by archived"
     and workspace_id = flag "--workspace-id" (optional string) ~doc:"fitler by workspace id"
     and parent_folder_id = flag "--parent-folder-id" (optional string) ~doc:"filter by parent folder id"
     and order_by = flag "--order-by" (optional string) ~doc:"filter by order by (created-at|updated-at)"
     in
     (fun () ->
       let instance = Boostnote.Instance.(make ~base_url ~token) in
       Boostnote.Documents.list instance ?title ~archived ?workspace_id ?parent_folder_id ?order_by ()
       |> Lwt_main.run
       |> (function
           | Ok body -> print_endline body
           | Error message -> message |> Piaf.Error.to_string |> print_endline)))

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
           | Error message -> message |> Piaf.Error.to_string |> print_endline)))

let documents =
  Command.group
    ~summary:"Commands about documents"
    [ "list", list_documents
    ; "get", get_document
    ]

let command =
  Command.group
    ~summary:"Boost Note"
    [ "documents", documents
    ]

let () =
  Command.run
    ~version:"0.0.1"
    command
