open Smartschool_soap

let () = Lwt_main.run (
  let access_code = read_line () in
  match%lwt get_all_users ~access_code ~recursive:true Sys.argv.(1) with
  | Error e -> failwith (Protocol_conv_json.Json.error_to_string_hum e)
  | Ok l ->
    let open Smartschool.Users in
    l |> List.iter (fun u ->
      let class_ = (List.find (fun g -> g.is_class) u.groups).code in
      let show (c : Coaccount.t) msg =
        Printf.printf "%s %s*%s, %s*%s (%s) : %s\n"
          class_ u.lastname u.firstname
          c.lastname c.firstname c.typ
          msg
      in
      let find_co f msg =
        let rec g i =
          if i = Array.length u.coaccounts then
            ()
          else if u.coaccounts.(i).typ <> "" && f u.coaccounts.(i) then
            show u.coaccounts.(i) msg
          else
            g (i + 1)
        in
        g 0
      in
      let open Coaccount in
      find_co (fun c -> c.typ = "Sélectionner") "type";
      let find_co_n f = find_co (fun c -> f c.firstname || f c.lastname) in
      find_co_n ((=) "") "nom vide";
      find_co_n (fun x -> String.contains x '\\') "nom cassé";
      find_co_n (fun x -> String.trim x <> x) "espaces en trop";
      find_co_n (fun x ->
        Str.(global_replace (regexp {|[Vv]an \|[Dd]e |}) "" x) |>
        String.trim |>
        String.split_on_char ' ' |>
        List.length > 1
      ) "nom long";
      Array.to_list u.coaccounts |>
        List.filter (fun c -> c.typ <> "") |>
        List.sort (fun c c' ->
          let f x = x.lastname, x.firstname in
          compare (f c) (f c')
        ) |>
        List.fold_left (fun acc c ->
          match acc with
          | None -> Some c
          | Some c' -> if c = c' then show c "doublon"; Some c
        ) None |>
        ignore
    );
    Lwt.return ()
)
