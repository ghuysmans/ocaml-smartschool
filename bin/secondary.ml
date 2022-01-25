open Smartschool_soap

let () = Lwt_main.run (
  let access_code = read_line () in
  match%lwt get_all_users ~access_code ~recursive:false Sys.argv.(1) with
  | Error e -> failwith (Protocol_conv_json.Json.error_to_string_hum e)
  | Ok l ->
    l |> List.iter (fun (u : Smartschool.Users.user) ->
      Printf.printf "%s %s: %s %s (%s), %s %s (%s)\n"
        u.firstname u.lastname
        u.coaccounts.(0).firstname u.coaccounts.(0).lastname u.coaccounts.(0).typ
        u.coaccounts.(1).firstname u.coaccounts.(1).lastname u.coaccounts.(1).typ
    );
    Lwt.return ()
  (*
  get_all_users ~access_code ~recursive:true "1CE" >|= print_endline
  *)
)
