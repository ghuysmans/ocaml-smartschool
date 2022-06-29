type t = bool

let of_xml_light_exn = function
  | Xml.(Element (_, _, [PCData "0"])) -> false
  | Xml.(Element (_, _, [PCData "1"])) -> true
  | _ -> failwith "Binary.of_xml_light_exn"

let to_xml_light b =
  Xml.(Element ("status", [], [PCData (if b then "1" else "0")]))
