type t = bool

let of_string = function
  | "0" -> false
  | "1" -> true
  | _ -> failwith "Binary.of_string"

let of_xml_light_exn = function
  | Xml.(Element (_, _, [PCData x])) -> of_string x
  | _ -> failwith "Binary.of_xml_light_exn"

let to_string b = if b then "1" else "0"

let to_xml_light b =
  Xml.Element ("status", [], [Xml.PCData (to_string b)])

let params = Params.Simple {fwd = to_string; bwd = of_string}
