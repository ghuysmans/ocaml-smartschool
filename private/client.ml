type context = {
  host: string;
  cookie: string;
  user_agent: string;
  ctx: Cohttp_lwt_unix.Client.ctx;
}

let hijack ~host ~cookie ~user_agent =
  {host; cookie; user_agent; ctx = Cohttp_lwt_unix.Net.init ()}

open Lwt.Infix

module Agenda = struct
  type lesson = Agenda.Query.Response_data.lesson
  type assignment = Agenda.Assignment.t

  let timestamp ~y ~m ~d =
    let open Unix in
    let tm = {
      tm_sec = 0; tm_min = 0; tm_hour = 7;
      tm_mday = d; tm_mon = m - 1; tm_year = y - 1900;
      tm_wday = 0; tm_yday = 0; tm_isdst = false;
    } in
    int_of_float (fst (mktime tm))

  let call {host; cookie; user_agent; ctx} xml =
    let uri =
      Uri.make
        ~scheme:"https" ~host
        ~path:"/index.php"
        ~query:["module", ["Agenda"]; "file", ["dispatcher"]]
        ()
    in
    let body =
      Uri.encoded_of_query ["command", [
        Xml.to_string (Api.Request.to_xml_light xml)
      ]] |>
      Cohttp_lwt.Body.of_string
    in
    let headers =
      let origin = Uri.make ~scheme:"https" ~host () |> Uri.to_string in
      Cohttp.Header.of_list [
        "content-type", "application/x-www-form-urlencoded; charset=UTF-8";
        "cookie", cookie;
        "origin", origin;
        "referer", origin;
        "user-agent", user_agent;
        "x-requested-with", "XMLHttpRequest";
      ]
    in
    Cohttp_lwt_unix.Client.post ~ctx ~body ~headers uri >>= fun (resp, body) ->
    if Cohttp.Response.status resp = `OK then
      Cohttp_lwt.Body.to_string body >|= Xml.parse_string
    else
      Lwt.fail_with "Agenda.call"

  open Agenda

  let lessons ctx ?class_ start end_ =
    call ctx (Query.Request.make ?class_ start end_) >|=
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

  let lessons_with_assignments ctx ?class_ start end_ =
    lessons ctx ?class_ start end_ >>=
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
end
