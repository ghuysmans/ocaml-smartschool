open Smartschool.Agenda
open Smartschool.Private

let () =
  let get_dates d m =
    let d, m = int_of_string d, int_of_string m in
    let start = Smartschool_soap.timestamp ~y:2022 ~m ~d in
    start, start + 60 * 60 * 24 * 5
  in
  match Sys.argv with
  | [| _; "query"; d; m |] ->
    let start, end_ = get_dates d m in
    let xml =
      Query.Request.make start end_ |>
      Request.to_xml_light |>
      Xml.to_string
    in
    Uri.encoded_of_query ["command", [xml]] |> print_endline;
  | [| _; "edit"; d; m; mom; les; subject; notes |] ->
    let start, end_ = get_dates d m in
    let moment_id = int_of_string mom in
    let color = "kiwi" in
    let lesson_id = int_of_string les in
    let xml =
      Edit.Request.make ~start ~end_ ~moment_id ~notes ~color ~lesson_id subject |>
      Request.to_xml_light |>
      Xml.to_string
    in
    Uri.encoded_of_query ["command", [xml]] |> print_endline;
  | [| _; "list" |] ->
    begin match Xml.parse_in stdin |> Query.Response.of_xml_light_exn with
    | {response = {actions = {l = [{data = {content = {lessons = {l}}}; _}]}; _}} ->
      l |> List.iter (fun (x : Query.Response_data.lesson) ->
        let moments = String.concat "-" (List.map string_of_int x.moments) in
        let lessons = String.concat "-" (List.map string_of_int x.lessons) in
        let courses = String.concat ", " x.courses in
        let classrooms = String.concat ", " x.classrooms in
        Printf.printf "mid=%s lid=%s %s @ %s\n" moments lessons courses classrooms
      )
    | _ ->
      failwith "meh"
    end
  | _ ->
    Printf.eprintf
      "usage: %s query d m | list | edit d m mid lid subject notes\n"
      Sys.argv.(0);
    exit 1
