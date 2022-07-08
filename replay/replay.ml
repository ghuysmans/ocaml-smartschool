type t = Smartschool_private.Request.command

let compare = compare
let sexp_of_t _ = Sexplib0.Sexp.Atom "FIXME"

let of_raw_command raw =
  let {Smartschool_private.Request.l} =
    Xml.parse_string raw |>
    Smartschool_private.Request.of_xml_light_exn
  in
  l

let of_har req resp =
  match req with
  | {Har.Entry.Request.post_data = Some {
        params = [{name = "command"; value = raw_commands; _}];
        _
      };
      _
    } ->
    begin match of_raw_command (Uri.pct_decode raw_commands) with
      | [command] ->
        (* fast path *)
        [command, Har_replay.(response_of_har resp, body_of_har resp)]
      | commands ->
        match resp.content with
        | Some {text = Some raw_response; _} ->
          begin match Xml.parse_string raw_response with
          | Xml.Element ("server", [], responses) ->
            List.combine commands  responses |>
            List.map (fun (command, response) ->
              let body =
                Xml.Element ("server", [], [response]) |>
                Xml.to_string |>
                Cohttp_lwt.Body.of_string
              in
              command, (Har_replay.response_of_har resp, body)
            )
          | _ ->
            []
          end
        | _ -> []
    end
  | _ -> []

let of_cohttp ?body _uri = function
  | {Cohttp.Request.meth = `POST; _} ->
    begin match body with
    | None -> failwith "empty body"
    | Some body ->
      match Uri.query_of_encoded body with
      | ["command", [raw]] -> List.hd (of_raw_command raw)
      | _ -> failwith "missing command"
    end
  | _ -> failwith "not a POST"
