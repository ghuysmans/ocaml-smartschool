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
      | Ok {l} -> Lwt.return l
      | Error e ->
        Lwt.fail_with (Protocol_conv_xml.Xml_light.error_to_string_hum e)
    end
  | `Unauthorized -> Lwt.fail_with "session expired"
  | x -> Lwt.fail_with @@ Cohttp.Code.string_of_status x

let call1 mod_ c command =
  call mod_ c [command] >>= function
    | [{Response.status = "ok"; actions = {l}}] -> Lwt.return l
    | _ -> Lwt.fail_with "Client.call1"

module Agenda = struct
  open Agenda

  type lesson = Query.Action_data.lesson
  type assignment = Assignment.t
  type assignment_type = Agenda.Assignment_type.t
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
  let call1 = call1 "Agenda"

  let lessons ctx ?filter start end_ =
    call1 ctx (Request.Lessons {start; end_; end_old = end_; filter}) >>= function
      | [Response.Lessons l] -> Lwt.return l
      | _ -> Lwt.fail_with "Agenda.lessons"

  let assignments ctx ?(class_ids=[]) ~lesson_id moment_id =
    call1 ctx (Request.Assignments {class_ids; lesson_id; moment_id}) >>= function
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

  let edit ctx ?filter ?(assignments=[]) ~start ~end_ ~moment_id ~lesson_id ?color ?note ?subject t =
    let req =
      let open Query.Action_data in
      Request.Lesson_edit {
        start;
        end_;
        xml = {
          moment_id;
          subjects = {
            subject = {
              text = Option.value subject ~default:t.subject;
              moment_id
            }
          };
          notes = {
            note = {
              text = Option.value note ~default:t.note;
              moment_id
            }
          };
          assignments;
          unique_ids = "";
        };
        lesson_id;
        color = Option.value color ~default:t.color;
        filter;
      }
    in
    call1 ctx req >>= function
      | [Response.Lessons _] -> Lwt.return ()
      | _ -> Lwt.fail_with "Agenda.edit"

  let stream ({base; ctx; _} as c) ?fn (n : Stream_file.Notification_data.content) =
    let uri =
      Stream_file.Request.(Params.to_assoc params {
        extension = n.extension;
        random = n.random;
        title = n.title;
        file_size = n.filesize;
      }) |>
      Uri.add_query_params' base
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

  let assignment_types ctx =
    call1 ctx Request.Assignment_types >>= function
      | [Response.Assignment_types l] -> Lwt.return l
      | _ -> Lwt.fail_with "Agenda.assignment_types"

  module Print = struct
    let teacher_list ?teacher ?fn ctx ~typ ~start ~end_ ~subject ~room ~start_moment ~note ~daily ~color ~empty =
      let req =
        let l = List.map (fun {Agenda.Assignment_type.id; _} -> id) typ in
        Request.Teacher_print {
          start; end_;
          assignment_types = {items = {l}};
          subject; room; start_moment; note; daily; color; empty;
          filter = Option.map (fun x -> Teacher x) teacher;
        }
      in
      call1 ctx req >>= function
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
  let call1 = call1 "Messages"

  let messages ctx box =
    call1 ctx (Request.Messages {box}) >>= function
      | [Response.Messages l; Unknown _; Unknown _] -> Lwt.return l
      | _ -> Lwt.fail_with "Postboxes.messages"

  let message ctx box id =
    call1 ctx (Request.Message {box; id}) >>= function
      | Response.Message m :: _ -> Lwt.return m
      | _ -> Lwt.fail_with "Postboxes.message"

  let attachments ctx box id =
    call1 ctx (Request.Attachments {box; id}) >>= function
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

  let delete ctx box id =
    call1 ctx (Request.Message_delete {box; id}) >>= function
      | [Response.Message_delete] -> Lwt.return ()
      | _ -> Lwt.fail_with "Postboxes.delete"
end
end
