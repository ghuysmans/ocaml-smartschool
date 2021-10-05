open Protocol_conv_xml
open Smartschool.Courses

let () =
  match Xml.parse_file Sys.argv.(1) |> of_xml_light with
  | Error e -> failwith (Xml_light.error_to_string_hum e)
  | Ok l ->
    l |> List.iter (fun (c : Course.t) ->
      let groups =
        c.student_groups.l |>
        List.map (fun (g : Student_group.t) -> "\"" ^ g.description ^ "\"") |>
        String.concat ", "
      in
      Printf.printf "%s%s: %s (%s)\n"
        (match c.status with Active -> "" | Inactive -> "* ")
        c.main_teacher.username c.name groups
    )
