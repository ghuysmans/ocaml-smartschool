open Smartschool.Ical

let weekday {Glical.Datetime.year; month; day; _} =
  let open Unix in
  let tm = {
    tm_sec = 0; tm_min = 0; tm_hour = 0;
    tm_mday = day; tm_mon = month - 1; tm_year = year - 1900;
    tm_wday = 0; tm_yday = 0; tm_isdst = false;
  } in
  match (snd @@ mktime tm).tm_wday with
  | 0 -> "Sun"
  | 1 -> "Mon"
  | 2 -> "Tue"
  | 3 -> "Wed"
  | 4 -> "Thu"
  | 5 -> "Fri"
  | 6 -> "Sat"
  | _ -> failwith "mktime"


let () =
  let l = Glical.file_contents Sys.argv.(1) |> parse in
  let show f lbl =
    print_endline @@ lbl ^ ":";
    List.map (fun x -> "- " ^ f x) l |>
      List.sort_uniq compare |>
      List.iter print_endline;
    print_newline ()
  in
  show (fun {class_; _} -> class_) "Classes";
  show (fun {subject; _} -> subject) "Subjects";
  show (fun {room; _} -> room) "Rooms";
  List.sort compare l |>
  List.iter (fun {event; class_; room; subject} ->
    Printf.printf "%s %s %s -> %s\t%s\t%s\t%s\n"
      (weekday event.dtstart)
      (string_of_date event.dtstart)
      (string_of_time event.dtstart)
      (string_of_time event.dtend)
      class_
      room
      subject
  )
