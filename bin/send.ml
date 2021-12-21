open Smartschool_soap

let () = Lwt_main.run (
  send_message ~access_code:(read_line ())
               ~from:"huysgu" ~to_:("huysgu", 0)
               ~title:"send.ml" Sys.argv.(1)
)
