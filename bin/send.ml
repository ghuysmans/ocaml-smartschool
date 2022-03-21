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
      let out = Csv.to_channel (open_out (d ^ ".log")) in
      Csv.output_record out ["result"; "user"; "coaccount"];
      let%lwt () = data |> Lwt_list.iter_s (fun row ->
        let get x = Hashtbl.find h x |> List.nth row in
        let user, coaccount, title = get "dest", get "dest_n", get "title" in
        Printf.eprintf "\r\027[0K%s:%s%!" user coaccount;
        let%lwt e =
          let to_ = user, int_of_string coaccount in
          List.map (Merge_omd.map get) template |>
          Omd.to_html |>
          send_message ~access_code ~from ~to_ ~title
        in
        let result =
          match e with
          | Ok () -> "ok"
          | Error `Access_code -> "accesscode"
          | Error `Account -> "account"
          | Error (`Code c) -> string_of_int c
          | Error `NaN -> "?"
        in
        Csv.output_record out [result; user; coaccount];
        Lwt.return_unit
      ) in
      Printf.eprintf "\r\027[0Kdone\n";
      Lwt.return_unit
    end
  | _ ->
    Printf.eprintf "usage: %s -from user -data data.csv -template tpl.md\n" Sys.argv.(0);
    exit 1
)
