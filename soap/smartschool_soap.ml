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

let send_message ~access_code ~from ~to_ ~title ?(lvs=false) ?(attachments=[]) body =
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
    with_envelope ~access_code ~request_element:"sendMsg" |>
    Cohttp_lwt.Body.of_string
  in
  let open Lwt.Infix in
  Cohttp_lwt_unix.Client.post ~body uri >>= fun (resp, _body) ->
  if Cohttp.Response.status resp = `OK then
    Lwt.return_unit
  else
    Lwt.fail_with "sendMsg"
