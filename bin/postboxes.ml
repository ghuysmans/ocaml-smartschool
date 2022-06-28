open Smartschool.Postboxes
open Smartschool.Private

let () =
  match Sys.argv with
  | [| _; "query"; box_type |] ->
    let xml =
      Query.Request.make box_type |>
      Request.to_xml_light |>
      Xml.to_string
    in
    Uri.encoded_of_query ["command", [xml]] |> print_endline;
  | [| _; "list" |] ->
    begin match Xml.parse_in stdin |> Query.Response.of_xml_light_exn with
    | {response = {actions = {l = {data = {messages = {l}}; _} :: _}; _}} ->
      l |> List.iter (fun (x : Query.Response_data.message) ->
        Printf.printf "id=%d %d %s (%s)\n" x.id x.attachments x.subject x.date
      )
    | _ ->
      failwith "meh"
    end
  | [| _; "lista" |] ->
    begin match Xml.parse_in stdin |> Query_attachments.Response.of_xml_light_exn with
    | {response = {actions = {l = {data = {attachments = {l}}; _} :: _}; _}} ->
      l |> List.iter (fun (x : Query_attachments.Response_data.attachment) ->
        Printf.printf "id=%d %s, %s (%s)\n" x.file_id x.name x.mime x.size;
        Query_attachments.url ~host:"example.com" x.file_id |>
          Uri.to_string |>
          Printf.printf "%s\n"
      )
    | _ ->
      failwith "meh"
    end
  | [| _; "fetch"; box_type; id |] ->
    let xml =
      Fetch_message.Request.make box_type (int_of_string id) |>
      Request.to_xml_light |>
      Xml.to_string
    in
    Uri.encoded_of_query ["command", [xml]] |> print_endline;
  | [| _; "querya"; box_type; id |] ->
    let xml =
      Query_attachments.Request.make box_type (int_of_string id) |>
      Request.to_xml_light |>
      Xml.to_string
    in
    Uri.encoded_of_query ["command", [xml]] |> print_endline;
  | [| _; "show" |] ->
    begin match Xml.parse_in stdin |> Fetch_message.Response.of_xml_light_exn with
    | {response = {actions = {l = [{data = {message = x}; _}]}; _}} ->
      Printf.printf "<!-- id=%d %d %s (%s) -->\n"
        x.id x.attachments x.subject x.date;
      Printf.printf "%s\n" x.body
    | _ ->
      failwith "meh"
    end
  | _ ->
    Printf.eprintf
      "usage:\t%s query {in,out}box | fetch *box id | list | show\n"
      Sys.argv.(0);
    Printf.eprintf "\t%s querya {in,out}box id | lista\n" Sys.argv.(0);
    exit 1
