open Protocol_conv_xml

module Params = struct
  type param = string * string

  let param_to_xml_light (name, value) =
    let v = if value = "" then [] else [Xml.PCData value] in
    Xml.Element ("param", ["name", name], v)

  let param_of_xml_light_exn = function
    | Xml.(Element ("param", ["name", name], [])) -> name, ""
    | Xml.(Element ("param", ["name", name], [PCData value])) -> name, value
    | _ -> failwith "Params.of_xml_light_exn"

  type t = {
    l: param list [@key "param"];
  } [@@deriving protocol ~driver:(module Xml_light)]
end

type command = {
  subsystem: string;
  action: string;
  params: Params.t;
} [@@deriving protocol ~driver:(module Xml_light)]

type t = {
  l: command list [@key "command"];
} [@@deriving protocol ~driver:(module Xml_light)]

let to_xml_light t =
  match to_xml_light t with
  | Xml.Element (_, _, ch) -> Xml.Element ("request", [], ch)
  | _ -> failwith "Request.to_xml_light"
