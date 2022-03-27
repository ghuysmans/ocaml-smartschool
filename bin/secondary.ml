open Smartschool_soap

let () = Lwt_main.run (
  let access_code = read_line () in
  match%lwt get_all_users ~access_code ~recursive:true Sys.argv.(1) with
  | Error e -> failwith (Protocol_conv_json.Json.error_to_string_hum e)
  | Ok l ->
    let open Smartschool.Users in
    let out = Csv.to_channel stdout in
    Csv.output_record out [
      "type";
      "official_class"; "lastname"; "firstname";
      "role"; "rel_lastname"; "rel_firstname";
      "username";
      "coaccount";
      "teacher";
      "error"
    ];
    let dummy = {
      Coaccount.firstname = "";
      lastname = "";
      email = "";
      email_verified = false;
      phone_number = "";
      mobile_number = "";
      typ = "";
      authenticator_app = false;
      yubikey = false;
      status = "";
    } in
    l |> List.iter (fun u ->
      let class_ =
        match List.filter (fun g -> g.is_class) u.groups with
        | [] -> "?"
        | g :: _ -> g.code
      in
      let show typ i (c : Coaccount.t) msg =
        let teacher =
          if class_ = "?" then
            if c == dummy then
              u.lastname ^ " " ^ u.firstname
            else
              c.lastname ^ " " ^ c.firstname
          else
            ""
        in
        Csv.output_record out [
          typ;
          class_; u.lastname; u.firstname;
          c.typ; c.lastname; c.firstname;
          u.username;
          string_of_int (i + 1);
          teacher;
          msg
        ]
      in
      let find_co f msg =
        let rec g i =
          if i = Array.length u.coaccounts then
            ()
          else if u.coaccounts.(i).typ <> "" && f u.coaccounts.(i) then
            show "lint" i u.coaccounts.(i) msg
          else
            g (i + 1)
        in
        g 0
      in
      let open Coaccount in
      show "dump" (-1) dummy "";
      Array.iteri (fun i c -> if c.typ <> "" then show "dump" i c "") u.coaccounts;
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
        List.mapi (fun i x -> i, x) |>
        List.filter (fun (_, c) -> c.typ <> "") |>
        List.sort (fun (_, c) (_, c') ->
          let f x = x.lastname, x.firstname in
          compare (f c) (f c')
        ) |>
        List.fold_left (fun acc (i, c) ->
          match acc with
          | None -> Some c
          | Some c' -> if c = c' then show "lint" i c "doublon"; Some c
        ) None |>
        ignore
    );
    Lwt.return ()
)
