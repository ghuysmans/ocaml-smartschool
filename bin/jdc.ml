open Smartschool.Ical

let () =
  file_contents Sys.argv.(1) |> parse |>
  List.iter (fun {dtstart = {year; month; day; hours; minutes; _};
                  dtend = {hours = h'; minutes = m'; _};
                  summary; location} ->
    Printf.printf "%02d/%02d/%d %02d:%02d -> %02d:%02d\t%s%s\n"
      day month year hours minutes h' m'
      summary
      (match location with
       | None -> ""
       | Some l -> " @ " ^ l)
  )
