open Smartschool.Ical

let () =
  file_contents Sys.argv.(1) |> parse |>
  List.iter (fun {dtstart; dtend; summary; location} ->
    Printf.printf "%s %s -> %s\t%s%s\n"
      (string_of_date dtstart) (string_of_time dtstart) (string_of_time dtend)
      summary
      (match location with
       | None -> ""
       | Some l -> " @ " ^ l)
  )
