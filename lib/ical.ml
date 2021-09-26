open Glical

type t = {
  (* FIXME extract the global X-WR-TIMEZONE? *)
  dtstart: Datetime.t;
  dtend: Datetime.t;
  summary: string;
  location: string option;
}

type course = {
  event: t;
  class_: string;
  subject: string;
  room: string;
}

let parse raw =
  match Lexing.lex_ical raw |> parse_ical |> Datetime.parse_datetime with
  | [Ical.Block (_, "VCALENDAR", events)] ->
    events |> List.fold_left (fun acc -> function
      | Ical.Block (_, "VEVENT", attrs) ->
        begin match
          let f location = function
            | {Ical.value = `Raw x; _} -> List.hd (text_of_raw location x)
            | _ -> failwith "parse_p"
          in
          attrs |> List.fold_left (fun (st, e, du, su, l) -> function
            | Ical.Assoc (_, "DTSTART", _, {value = `Datetime d; _}) -> Some d, e, du, su, l
            | Ical.Assoc (_, "DTEND", _, {value = `Datetime d; _}) -> st, Some d, du, su, l
            | Ical.Assoc (p, "DURATION", _, v) -> st, e, Some (f p v), su, l
            | Ical.Assoc (p, "SUMMARY", _, v) -> st, e, du, Some (f p v), l
            | Ical.Assoc (p, "LOCATION", _, v) -> st, e, du, su, Some (f p v)
            | _ -> st, e, du, su, l
          ) (None, None, None, None, None)
        with
        | Some dtstart, Some dtend, None, Some summary, location ->
          {dtstart; dtend; summary; location} :: acc
        | Some dtstart, None, Some _duration, Some summary, location ->
          {dtstart; dtend = failwith "FIXME add duration"; summary; location} :: acc
        | None, _, _, _, _ -> acc (* ignore all-day events *)
        (* FIXME keep track of the location *)
        | _, Some _, Some _, _, _ -> failwith "DTEND ^ DURATION"
        | _ -> failwith "incomplete event"
        end
      | _ -> acc
    ) []
  | _ -> failwith "not a calendar"

let of_smartschool =
  List.filter_map (fun event ->
    match String.split_on_char ',' event.summary |> List.map String.trim with
    | [class_; subject; room] -> Some {event; class_; subject; room}
    | _ -> None (* probably a holiday *)
  )
