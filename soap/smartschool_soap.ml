let uri = Uri.of_string "https://isfath.smartschool.be/Webservices/V3"

let with_envelope ~access_code ~request_element body =
  Printf.sprintf {|
    <soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xmlns:xsd="http://www.w3.org/2001/XMLSchema"
    xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
    xmlns:v3="https://isfath.smartschool.be/Webservices/V3">
    <soapenv:Header/>
    <soapenv:Body>
    <v3:%s soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
    <accesscode>%s</accesscode>
    %s</v3:%s>
    </soapenv:Body>
    </soapenv:Envelope>
  |} request_element access_code body request_element


type attachment = {
  filename: string;
  data: string [@key "filedata"];
} [@@deriving to_yojson]

let escape x = Xml.(to_string (PCData x))

let extract ~request_element raw =
  match Xml.parse_string raw with
  | Xml.Element ("SOAP-ENV:Envelope", _, [
      Element ("SOAP-ENV:Body", _, [
      Element (t, _, [
      Element ("return", _, [PCData x])])])])
      when t = "ns1:" ^ request_element ^ "Response" ->
    Lwt.return x
  | _ -> Lwt.fail_with "extract"

let send_message ~access_code ~from ~to_ ~title ?(lvs=false) ?(attachments=[]) body =
  let request_element = "sendMsg" in
  let body =
    let uid, coaccount = to_ in
    let attachments =
      attachments |>
      List.map (fun a -> {a with data = Base64.encode_string a.data}) |>
      [%to_yojson: attachment list] |>
      Yojson.Safe.to_string
    in
    let lvs = if lvs then "true" else "false" in
    Printf.sprintf {|
      <userIdentifier>%s</userIdentifier>
      <title>%s</title>
      <body>%s</body>
      <senderIdentifier>%s</senderIdentifier>
      <attachments>%s</attachments>
      <coaccount>%d</coaccount>
      <copyToLVS>%s</copyToLVS>
    |} uid (escape title) (escape body) from attachments coaccount lvs |>
    with_envelope ~access_code ~request_element |>
    Cohttp_lwt.Body.of_string
  in
  let open Lwt.Infix in
  Cohttp_lwt_unix.Client.post ~body uri >>= fun (resp, body) ->
  if Cohttp.Response.status resp = `OK then
    Cohttp_lwt.Body.to_string body >>=
    extract ~request_element >|=
    Yojson.Safe.from_string >|= function
      | `Int 0 -> Ok ()
      | `Int 8 -> Error `Access_code
      | `Int 12 -> Error `Account
      | `Int e -> Error (`Code e)
      | _ -> Error `NaN
  else
    Lwt.fail_with "sendMsg"


let get_user ~access_code username =
  let body =
    Printf.sprintf "<username>%s</username>" (escape username) |>
    with_envelope ~access_code ~request_element:"getUserDetailsByUsername" |>
    Cohttp_lwt.Body.of_string
  in
  let open Lwt.Infix in
  Cohttp_lwt_unix.Client.post ~body uri >>= fun (resp, body) ->
  if Cohttp.Response.status resp = `OK then
    Cohttp_lwt.Body.to_string body >>=
    extract ~request_element:"getUserDetailsByUsername" >|=
    Yojson.Safe.from_string >|=
    Smartschool.Users.user_of_json
  else
    Lwt.fail_with "getUserDetailsByUsername"

let get_all_users ~access_code ~recursive code =
  let body =
    let recu = if recursive then 1 else 0 in
    Printf.sprintf "<code>%s</code><recursive>%d</recursive>" (escape code) recu |>
    with_envelope ~access_code ~request_element:"getAllAccountsExtended" |>
    Cohttp_lwt.Body.of_string
  in
  let open Lwt.Infix in
  Cohttp_lwt_unix.Client.post ~body uri >>= fun (resp, body) ->
  if Cohttp.Response.status resp = `OK then
    Cohttp_lwt.Body.to_string body >>=
    extract ~request_element:"getAllAccountsExtended" >|=
    Yojson.Safe.from_string >|=
    Smartschool.Users.of_json
  else
    Lwt.fail_with "getAllAccountsExtended"
