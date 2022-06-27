open Smartschool.Agenda.Query

let () =
  let t = Xml.parse_in stdin |> Response.of_xml_light_exn in
  (List.hd t.response.actions.l).data.content.l.l |>
  List.iter (fun Response.{moment_id; lesson_id; date; course; classroom; _} ->
    Printf.printf "%d, %d, %s, %s, %s\n"
      moment_id lesson_id date course classroom
  )
