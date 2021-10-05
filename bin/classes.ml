open Protocol_conv_json
open Smartschool.Classes

let () =
  match of_json (Yojson.Safe.from_file Sys.argv.(1)) with
  | Error e -> failwith (Json.error_to_string_hum e)
  | Ok l ->
    l |> List.iter (fun c ->
      Printf.printf "%s : %s, %sofficial\n"
        c.code c.untis (if c.official then "" else "un")
    )
