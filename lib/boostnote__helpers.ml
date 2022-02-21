let body_of_list : (string * string option) list -> (string * string) list =
  List.filter_map (fun (key, opt) ->
      match opt with
      | None -> None
      | Some value -> Some (key, value))
