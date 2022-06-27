let () =
  Printf.printf "%d\n" (Smartschool_soap.timestamp ~y:2022 ~m:6 ~d:6)
  (*
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
  *)
