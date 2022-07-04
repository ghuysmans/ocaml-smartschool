open Protocol_conv_xml

module Params = struct
  type param = string * string
  type 'a derived = 'a -> param list
  type 'a attribute = Key of string (* TODO default *)

  module D = struct
    let key x = Key x
    let params_int x = ["", string_of_int x]
    let params_string x = ["", x]
  end

  module T = struct
    type nonrec 'a t = 'a derived
    type nonrec 'a attribute = 'a attribute
  end

  let apply_iso t _ b x = t (b x)

  open Ppx_type_directed_value_runtime.Type_directed

  let rec of_record : type a len. (a, len) Record(T).t -> a T.t = fun r ->
    let m v x =
      let name =
        match v.Key.attribute with
        | Some (Key k) -> k
        | None -> v.name
      in
      List.map (fun (_, s) -> name, s) (v.value x)
    in
    match r with
    | [ v ]               -> fun (x, ()) -> m v x
    | v :: (_ :: _ as tl) -> fun (x, y) -> m v x @ of_record tl y

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
