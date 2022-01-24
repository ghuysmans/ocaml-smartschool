open Smartschool_soap

let () = Lwt_main.run (
  let access_code = read_line () in
  match%lwt get_all_users ~access_code ~recursive:false "1CE" with
  | Error e -> failwith (Protocol_conv_json.Json.error_to_string_hum e)
  | Ok l ->
    l |> List.iter (fun u ->
      Printf.printf "%s %s: %s %s, %s %s\n"
        u.Smartschool.Users.firstname u.lastname
        u.coaccount1_firstname u.coaccount1_lastname
        u.coaccount2_firstname u.coaccount2_lastname
    );
    Lwt.return ()
  (*
  get_all_users ~access_code ~recursive:true "1CE" >|= print_endline
  *)
)
