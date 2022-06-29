open Smartschool_private.Agenda
open Smartschool_private.Api

let () =
  let get_dates d m =
    let d, m = int_of_string d, int_of_string m in
    let start = timestamp ~y:2022 ~m ~d in
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
        let s = if x.assignment_end || x.test_deadline then "*" else "" in
        Printf.printf
          "mid=%s lid=%s %s @ %s%s\n\t%s\n"
          moments lessons courses classrooms s
          x.subject
      )
    | _ ->
      failwith "meh"
    end
  | [| _; "querya"; mom; les |] ->
    let moment_id = int_of_string mom in
    let lesson_id = int_of_string les in
    let xml =
      Assignment.Request.make ~lesson_id moment_id |>
      Request.to_xml_light |>
      Xml.to_string
    in
    Uri.encoded_of_query ["command", [xml]] |> print_endline;
  | [| _; "lista" |] ->
    begin match Xml.parse_in stdin |> Assignment.Response.of_xml_light_exn with
    | {response = {actions = {l = [{data = {form = {a = {l}}}; _}]}; _}} ->
      l |> List.iter (fun (x : Assignment.t) ->
        Printf.printf "id=%d %s (%s)\n"
          x.assignment_id x.description x.assigned_date
      )
    | _ ->
      failwith "meh"
    end
  | _ ->
    Printf.eprintf
      "usage:\t%s query d m | list | edit d m mid lid subject notes\n"
      Sys.argv.(0);
    Printf.eprintf "\t%s querya mid lid | lista\n" Sys.argv.(0);
    exit 1
