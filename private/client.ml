module Make (C : Cohttp_lwt.S.Client) = struct
type context = {
  base: Uri.t;
  cookie: string;
  user_agent: string;
  ctx: C.ctx;
}

let hijack ~host ~user_agent ctx =
  let base = Uri.make ~scheme:"https" ~host ~path:"/index.php" () in
  let cookie =
    match Sys.getenv_opt "SMSC_COOKIE" with
    | None ->
      prerr_endline "I need your cookie to continue.";
      prerr_endline "Store it in the SMSC_COOKIE envvar or paste it below:";
      read_line ()
    | Some c -> c
  in
  {base; cookie; user_agent; ctx}

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

let call mod_ ({ctx; base; _} as c) l =
  let uri = Uri.with_query' base ["module", mod_; "file", "dispatcher"] in
  let params = ["command", [Xml.to_string (Request.(to_xml_light {l}))]] in
  let headers = headers c in
  C.post_form ~ctx ~headers ~params uri >>= fun (resp, body) ->
  match Cohttp.Response.status resp with
  | `OK ->
    Cohttp_lwt.Body.to_string body >|=
    Xml.parse_string >|=
    Response.of_xml_light >>= begin function
      | Ok {response = {status = "ok"; actions = {l}}} -> Lwt.return l
      | Ok {response = {status; _}} -> Lwt.fail_with status
      | Error e ->
        Lwt.fail_with (Protocol_conv_xml.Xml_light.error_to_string_hum e)
    end
  | `Unauthorized -> Lwt.fail_with "session expired"
  | x -> Lwt.fail_with @@ Cohttp.Code.string_of_status x

module Agenda = struct
  open Agenda

  type lesson = Query.Action_data.lesson
  type assignment = Assignment.t
  type nonrec filter = filter
  type command = Request.command

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
    call ctx [Query.Command.make ?filter start end_] >>= function
      | [Response.Lessons l] -> Lwt.return l
      | _ -> Lwt.fail_with "Agenda.lessons"

  let assignments ctx ~lesson_id moment_id =
    call ctx [Assignment.Command.make ~lesson_id moment_id] >>= function
      | [Response.Assignments l] -> Lwt.return l
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
      let open Query.Action_data in
      Edit.Command.make
        ~start ~end_
        ~moment_id
        ~lesson_id
        ~color:(Option.value color ~default:t.color)
        ~note:(Option.value note ~default:t.note)
        (Option.value subject ~default:t.subject)
    in
    call ctx [req] >>= function
      | [Response.Lessons _] -> Lwt.return ()
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
    C.get ~ctx ~headers uri >>= fun (resp, body) ->
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
      call ctx [Print.Teacher_list.Command.make ~start ~end_ ~subject ~room ~start_moment ~note ~daily ~color ~empty teacher] >>= function
        | [Response.Agenda_notification x] -> stream ctx ?fn x
        | _ -> Lwt.fail_with "Agenda.Print.teacher_list"
  end
end

module Postboxes = struct
  open Postboxes

  type nonrec box = Box.t
  type item = Query.Action_data.message
  type message = Fetch_message.Action_data.message
  type attachment = Query_attachments.Action_data.attachment
  type command = Request.command

  let box_of_string = Box.of_string

  let call = call "Messages"

  let messages ctx b =
    call ctx [Query.Command.make b] >>= function
      | [Response.Messages l; Unknown _; Unknown _] -> Lwt.return l
      | _ -> Lwt.fail_with "Postboxes.messages"

  let message ctx b id =
    call ctx [Fetch_message.Command.make b id] >>= function
      | [Response.Message m] -> Lwt.return m
      | _ -> Lwt.fail_with "Postboxes.message"

  let attachments ctx b id =
    call ctx [Query_attachments.Command.make b id] >>= function
      | [Response.Attachments l] -> Lwt.return l
      | _ -> Lwt.fail_with "Postboxes.attachments"

  let attachment_uri {base; _} {Query_attachments.Action_data.file_id; _} =
     Query_attachments.uri ~host:(Uri.host_with_default base) file_id

  let attachment ({base; ctx; user_agent; _} as c) ?fn a =
    let uri = attachment_uri c a in
    let headers = headers c in
    C.get ~ctx ~headers uri >>= fun (resp, body) ->
    Cohttp_lwt.Body.drain_body body >>= fun () ->
    match Cohttp.Response.status resp with
    | `Found ->
      begin match Cohttp.Header.get_location resp.headers with
      | None -> Lwt.fail_with "missing Location"
      | Some uri ->
        let headers =
          Cohttp.Header.of_list [
            "referer", Uri.with_path base "/" |> Uri.to_string;
            "user-agent", user_agent;
          ]
        in
        C.get ~ctx ~headers uri >>= fun (resp, body) ->
        match Cohttp.Response.status resp with
        | `OK ->
          let fn =
            match fn with
            | Some x -> Ok x
            | None ->
              match Cohttp.Header.get resp.headers "content-disposition" with
              | None -> Error "missing content-disposition"
              | Some x ->
                let open Multipart_form.Content_disposition in
                match of_string (x ^ "\r\n") with
                | Error (`Msg e) -> Error e
                | Ok cd ->
                  match filename cd with
                  | Some x -> Ok x
                  | None -> Error "missing filename"
          in
          begin match fn with
          | Error e -> Lwt.fail_with e
          | Ok fn ->
            Lwt_io.(open_file ~mode:Output fn) >>= fun out ->
            Cohttp_lwt.Body.write_body (Lwt_io.write out) body >>= fun () ->
            Lwt_io.close out >>= fun () ->
            Lwt.return fn
          end
        | x -> Lwt.fail_with @@ Cohttp.Code.string_of_status x
      end
    | x -> Lwt.fail_with @@ Cohttp.Code.string_of_status x

  let delete ctx b id =
    call ctx [Delete.Command.make b id] >>= function
      | [Response.Message_delete] -> Lwt.return ()
      | _ -> Lwt.fail_with "Postboxes.delete"
end
end
