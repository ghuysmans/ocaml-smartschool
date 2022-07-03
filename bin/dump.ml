let () =
  match Yojson.Safe.from_channel stdin |> Har.of_yojson with
  | Error e ->
    print_endline e;
    exit 1
  | Ok har ->
    har.log.entries |> List.iter (function
      | {
          Har.Entry.request = {
            url;
            post_data = Some {
              params = [{name = "command"; value = request; _}];
              _
            };
            _
          };
          response = {
            content = Some {
              mime_type = "application/xml";
              text = Some raw_response;
              _
            };
            _
          };
          _
        } when Uri.host url = Some Net_config.host ->
        let response = Xml.(parse_string raw_response |> to_string_fmt) in
        Printf.printf "POST %s\n%s\n->\n%s\n\n"
          (Uri.to_string url)
          (Uri.pct_decode request)
          response
      | _ -> ()
    )
