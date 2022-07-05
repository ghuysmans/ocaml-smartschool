module C = Smartschool_private.Client.Make (Cohttp_lwt_unix.Client)
open C

let () = Lwt_main.run (
  let get_ctx () =
    Cohttp_lwt_unix.Net.init () |>
    Net_config.(hijack ~host ~user_agent)
  in
  match Sys.argv with
  | [| _; "query"; b |] ->
    let ctx = get_ctx () in
    let box = Postboxes.box_of_string b in
    let%lwt l = Postboxes.messages ctx box in
    l |> Lwt_list.iter_s (fun (x : Postboxes.item) ->
      Lwt_io.printf "id=%d %d %s (%s)\n" x.id x.attachments x.subject x.date
    )
  | [| _; "fetch"; b; id |] ->
    let ctx = get_ctx () in
    let id = int_of_string id in
    let box = Postboxes.box_of_string b in
    let%lwt m = Postboxes.message ctx box id in
    let%lwt l =
      if m.attachments > 0 then
        Postboxes.attachments ctx box id
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
  | [| _; "delete"; b; id |] ->
    let ctx = get_ctx () in
    let box = Postboxes.box_of_string b in
    let id = int_of_string id in
    let%lwt () = Postboxes.delete ctx box id in
    Lwt_io.printl "done"
  | [| _; "download"; b; id |] ->
    let ctx = get_ctx () in
    let id = int_of_string id in
    let box = Postboxes.box_of_string b in
    let%lwt l = Postboxes.attachments ctx box id in
    Lwt_list.iter_s (fun a ->
      let%lwt fn = Postboxes.attachment ctx a in
      Lwt_io.printl fn
    ) l
  | _ ->
    Printf.eprintf
      "usage:\t%s query {in,out}box | (fetch|delete|download) *box id\n"
      Sys.argv.(0);
    exit 1
)
