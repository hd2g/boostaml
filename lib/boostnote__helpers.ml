module Syntax = struct
  let ( >> ) f g x = g (f x)
  let ( << ) f g x = f (g x)
end

let body_of_list : (string * string option) list -> (string * string) list =
  List.filter_map (fun (key, opt) ->
      match opt with
      | None -> None
      | Some value -> Some (key, value))

let process_bool_string_exn =
  Syntax.(
    String.lowercase_ascii
    >> (function
        | "true" | "false" as value -> value
        | otherwise -> raise @@ Invalid_argument otherwise))
