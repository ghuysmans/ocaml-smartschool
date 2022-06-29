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

module Request = struct
  type command = {
    subsystem: string;
    action: string;
    params: Params.t;
  } [@@deriving protocol ~driver:(module Xml_light)]

  type t = {
    command: command;
  } [@@deriving protocol ~driver:(module Xml_light)]

  let to_xml_light t =
    match to_xml_light t with
    | Xml.Element (_, _, ch) -> Xml.Element ("request", [], ch)
    | _ -> failwith "Request.to_xml_light"
end

module Response (D : sig
  type data [@@deriving of_protocol ~driver:(module Xml_light)]
end) = struct
  type action = {
    subsystem: string;
    command: string;
    data: D.data;
  } [@@deriving of_protocol ~driver:(module Xml_light)]

  type actions = {
    l: action list [@key "action"];
  } [@@deriving of_protocol ~driver:(module Xml_light)]

  type response = {
    status: string;
    actions: actions;
  } [@@deriving of_protocol ~driver:(module Xml_light)]

  type t = { (* server *)
    response: response;
  } [@@deriving of_protocol ~driver:(module Xml_light)]
end

let timestamp ~y ~m ~d =
  let open Unix in
  let tm = {
    tm_sec = 0; tm_min = 0; tm_hour = 7;
    tm_mday = d; tm_mon = m - 1; tm_year = y - 1900;
    tm_wday = 0; tm_yday = 0; tm_isdst = false;
  } in
  int_of_float (fst (mktime tm))
