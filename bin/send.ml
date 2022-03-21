open Smartschool_soap

let () = Lwt_main.run (
  match Sys.argv with
  | [| _; "-from"; from; "-data"; d; "-template"; t |] ->
    begin match Csv.load d with
    | [] -> Lwt.return_unit
    | header :: data ->
      let access_code = read_line () in
      let h = Hashtbl.create 10 in
      header |> List.iteri (fun i v -> Hashtbl.replace h v i);
      let template = Omd.of_channel (open_in t) in
      data |> Lwt_list.iter_s (fun row ->
        let get x = Hashtbl.find h x |> List.nth row in
        let to_ = get "dest", int_of_string (get "dest_n") in
        let title = get "title" in
        let%lwt () = Lwt_io.printl (fst to_) in
        List.map (Merge_omd.map get) template |>
        Omd.to_html |>
        send_message ~access_code ~from ~to_ ~title
      )
    end
  | _ ->
    Printf.eprintf "usage: %s -from user -data data.csv -template tpl.md\n" Sys.argv.(0);
    exit 1
)
