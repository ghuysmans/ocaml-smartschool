type t = string list

let of_xml_light_exn = function
  | Xml.Element (_, _, []) -> []
  | Xml.(Element (_, _, [PCData l])) ->
    String.split_on_char ',' l |>
    List.map String.trim (* FIXME? *)
  | _ -> failwith "Names.of_xml_light_exn"

let to_xml_light l =
  Xml.(Element ("names", [], [PCData (String.concat "," l)]))
