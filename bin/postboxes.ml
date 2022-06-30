open Smartschool_private.Client

let () = Lwt_main.run (
  let get_ctx () = Net_config.(hijack ~host ~user_agent) in
  match Sys.argv with
  | [| _; "query"; box_type |] ->
    let ctx = get_ctx () in
    let%lwt l = Postboxes.messages ctx box_type in
    l |> Lwt_list.iter_s (fun (x : Postboxes.item) ->
      Lwt_io.printf "id=%d %d %s (%s)\n" x.id x.attachments x.subject x.date
    )
  | [| _; "fetch"; box_type; id |] ->
    let ctx = get_ctx () in
    let id = int_of_string id in
    let%lwt m = Postboxes.message ctx box_type id in
    let%lwt l =
      if m.attachments > 0 then
        Postboxes.attachments ctx box_type id
      else
        Lwt.return []
    in
    let%lwt () =
      Lwt_io.printf "<!-- id=%d %d %s (%s) -->\n%s\n"
        m.id m.attachments m.subject m.date
        m.body
    in
    l |> Lwt_list.iter_s (fun (x : Postboxes.attachment) ->
      Lwt_io.printf "id=%d %s, %s (%s)\n%s\n"
        x.file_id x.name x.mime x.size
        (Postboxes.attachment_uri ctx x |> Uri.to_string)
    )
  | _ ->
    Printf.eprintf
      "usage:\t%s query {in,out}box | fetch *box id\n"
      Sys.argv.(0);
    exit 1
)
