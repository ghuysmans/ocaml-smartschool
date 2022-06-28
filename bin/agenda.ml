open Smartschool.Agenda

let () =
  let xml =
    let start = 1654525702 in
    let end_ = 1654957702 in
    let moment_id = 848590 in
    let notes = "note" in
    let color = "kiwi" in
    let lesson_id = 757 in
    Edit.Request.make ~start ~end_ ~moment_id ~notes ~color ~lesson_id "test trois" |>
    Request.to_xml_light |>
    Xml.to_string
  in
  Uri.encoded_of_query ["command", [xml]] |> print_endline;
  match Xml.parse_in stdin |> Query.Response.of_xml_light_exn with
  | {response = {actions = {l = [{data = {content = {lessons = {l}}}; _}]}; _}} ->
    l |> List.iter (fun (x : Query.Response.lesson) ->
      let moments = String.concat "-" (List.map string_of_int x.moments) in
      let courses = String.concat ", " x.courses in
      let classrooms = String.concat ", " x.classrooms in
      Printf.printf "%s: %s @ %s\n" moments courses classrooms
    )
  | _ ->
    failwith "meh"
