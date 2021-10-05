open Smartschool.Attendance

let () =
  match of_yojson (Yojson.Safe.from_file Sys.argv.(1)) with
  | Error e -> failwith e
  | Ok l ->
    l |> List.iter (fun (x, {am; pm}) ->
      let f = Status.to_french in
      Printf.printf "%s : %s, %s\n" x (f am) (f pm)
    )
