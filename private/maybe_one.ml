type t = bool

let of_xml_light_exn = function
  | Xml.(Element (_, _, [])) -> false
  | Xml.(Element (_, _, [PCData "1"])) -> true
  | _ -> failwith "Maybe_one.of_xml_light_exn"

let to_xml_light b =
  Xml.(Element ("flag", [], [PCData (if b then "1" else "")]))
