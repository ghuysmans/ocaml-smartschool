open Smartschool.Agenda.Query

let () =
  let t = Xml.parse_in stdin |> Request.of_xml_light_exn in
  t.command.params.l |> List.iter (fun Request.Param.{name; value} ->
    Printf.printf "%s -> %s\n" name value
  )
