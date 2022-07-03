type context = {
  base: Uri.t;
  cookie: string;
  user_agent: string;
  ctx: Cohttp_lwt_unix.Client.ctx;
}

let hijack ~host ~user_agent =
  let base = Uri.make ~scheme:"https" ~host ~path:"/index.php" () in
  let cookie =
    match Sys.getenv_opt "SMSC_COOKIE" with
    | None ->
      prerr_endline "I need your cookie to continue.";
      prerr_endline "Store it in the SMSC_COOKIE envvar or paste it below:";
      read_line ()
    | Some c -> c
  in
  {base; cookie; user_agent; ctx = Cohttp_lwt_unix.Net.init ()}

open Lwt.Infix

let headers {base; cookie; user_agent; _} =
  let origin = Uri.with_path base "/" |> Uri.to_string in
  Cohttp.Header.of_list [
    "content-type", "application/x-www-form-urlencoded; charset=UTF-8";
    "cookie", cookie;
    "origin", origin;
    "referer", origin;
    "user-agent", user_agent;
    "x-requested-with", "XMLHttpRequest";
  ]

let call mod_ ({ctx; base; _} as c) xml =
  let uri = Uri.with_query' base ["module", mod_; "file", "dispatcher"] in
  let params = ["command", [Xml.to_string (Api.Request.to_xml_light xml)]] in
  let headers = headers c in
  Cohttp_lwt_unix.Client.post_form ~ctx ~headers ~params uri >>= fun (resp, body) ->
  match Cohttp.Response.status resp with
  | `OK -> Cohttp_lwt.Body.to_string body >|= Xml.parse_string
  | `Unauthorized -> Lwt.fail_with "session expired"
  | x -> Lwt.fail_with @@ Cohttp.Code.string_of_status x

module Agenda = struct
  open Agenda

  type lesson = Query.Response_data.lesson
  type assignment = Assignment.t
  type nonrec filter = filter

  let timestamp ~y ~m ~d =
    let open Unix in
    let tm = {
      tm_sec = 0; tm_min = 0; tm_hour = 7;
      tm_mday = d; tm_mon = m - 1; tm_year = y - 1900;
      tm_wday = 0; tm_yday = 0; tm_isdst = false;
    } in
    int_of_float (fst (mktime tm))

  let call = call "Agenda"

  let lessons ctx ?filter start end_ =
    call ctx (Query.Request.make ?filter start end_) >|=
    Query.Response.of_xml_light_exn >>= function
      | {response = {actions = {l = [{data = {content = {lessons = {l}}}; _}]}; _}} ->
        Lwt.return l
      | _ -> Lwt.fail_with "Agenda.lessons"

  let assignments ctx ~lesson_id moment_id =
    call ctx (Assignment.Request.make ~lesson_id moment_id) >|=
    Assignment.Response.of_xml_light_exn >>= function
      | {response = {actions = {l = [{data = {form = {a = {l}}}; _}]}; _}} ->
        Lwt.return l
      | _ -> Lwt.fail_with "Agenda.assignments"

  let lessons_with_assignments ctx ?filter start end_ =
    lessons ctx ?filter start end_ >>=
    Lwt_list.map_s (fun (l : lesson) ->
      (* FIXME this is inefficient *)
      if l.test_deadline || l.assignment_end then
        List.(map (fun x -> l.moments |> map (fun y -> x, y)) l.lessons |> concat) |>
        Lwt_list.map_s (fun (lesson_id, moment_id) ->
          assignments ctx ~lesson_id moment_id
        ) >>= fun a ->
        Lwt.return (l, List.concat a)
      else
        Lwt.return (l, [])
    )

  let edit ctx ~start ~end_ ~moment_id ~lesson_id ?color ?note ?subject t =
    let req =
      let open Query.Response_data in
      Edit.Request.make
        ~start ~end_
        ~moment_id
        ~lesson_id
        ~color:(Option.value color ~default:t.color)
        ~note:(Option.value note ~default:t.note)
        (Option.value subject ~default:t.subject)
    in
    call ctx req >|=
    Edit.Response.of_xml_light_exn >>= function
      | {response = {status = "ok"; _}} ->
        Lwt.return ()
      | _ -> Lwt.fail_with "Agenda.edit"

  let stream ({base; ctx; _} as c) ?fn (n : Stream_file.Notification_data.content) =
    let uri =
      Uri.add_query_params' base [
        "module", "Agenda";
        "file", "stream";
        "function", "streamFile";
        "extension", n.extension;
        "random", n.random;
        "title", n.title;
        "filesize", string_of_int n.filesize;
      ]
    in
    let headers = headers c in
    Cohttp_lwt_unix.Client.get ~ctx ~headers uri >>= fun (resp, body) ->
    match Cohttp.Response.status resp with
    | `OK ->
      let fn = Option.value ~default:n.title fn in
      Lwt_io.(open_file ~mode:Output fn) >>= fun out ->
      Cohttp_lwt.Body.write_body (Lwt_io.write out) body >>= fun () ->
      Lwt_io.close out >>= fun () ->
      Lwt.return fn
    | `Unauthorized -> Lwt.fail_with "session expired"
    | x -> Lwt.fail_with @@ Cohttp.Code.string_of_status x

  module Print = struct
    let teacher_list ?teacher ?fn ctx ~start ~end_ ~subject ~room ~start_moment ~note ~daily ~color ~empty =
      call ctx (Print.Teacher_list.Request.make ~start ~end_ ~subject ~room ~start_moment ~note ~daily ~color ~empty teacher) >|=
      Stream_file.Notification.of_xml_light_exn >>= function
        | {response = {actions = {l = [{data = {content}; _}]}; _}} ->
          stream ctx ?fn content
        | _ -> Lwt.fail_with "Agenda.Print.teacher_list"
  end
end

module Postboxes = struct
  open Postboxes

  type nonrec box_type = box_type
  type item = Query.Response_data.message
  type message = Fetch_message.Response_data.message
  type attachment = Query_attachments.Response_data.attachment

  let call = call "Messages"

  let messages ctx b =
    call ctx (Query.Request.make b) >|=
    Query.Response.of_xml_light_exn >>= function
      | {response = {actions = {l = {data = {messages = {l}}; _} :: _}; _}} ->
        Lwt.return l
      | _ -> Lwt.fail_with "Postboxes.messages"

  let message ctx b id =
    call ctx (Fetch_message.Request.make b id) >|=
    Fetch_message.Response.of_xml_light_exn >>= function
      | {response = {actions = {l = [{data = {message}; _}]}; _}} ->
        Lwt.return message
      | _ -> Lwt.fail_with "Postboxes.message"

  let attachments ctx b id =
    call ctx (Query_attachments.Request.make b id) >|=
    Query_attachments.Response.of_xml_light_exn >>= function
      | {response = {actions = {l = [{data = {attachments = {l}}; _}]}; _}} ->
        Lwt.return l
      | _ -> Lwt.fail_with "Postboxes.attachments"

  let attachment_uri {base; _} {Query_attachments.Response_data.file_id; _} =
     Query_attachments.uri ~host:(Uri.host_with_default base) file_id

  let delete ctx b id =
    let req = Delete.Request.make b id in
    call ctx req >|=
    Delete.Response.of_xml_light_exn >>= function
      | {response = {status = "ok"; _}} ->
        Lwt.return ()
      | _ -> Lwt.fail_with "Postboxes.delete"
end
