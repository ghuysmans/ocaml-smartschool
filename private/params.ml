open Protocol_conv_xml

type param = string * string
type 'a iso =
  | Simple of {to_string: 'a -> string; of_string: string -> 'a}
  | Complex of {fwd: 'a -> param list; bwd: param list -> 'a}
type 'a attribute = Key of string

module D = struct
  let key x = Key x

  let params_bool =
    Simple {
      to_string = (fun x -> if x then "true" else "false");
      of_string = (function
        | "true" -> true
        | "false" -> false
        | _ -> failwith "params_bool"
      )
    }

  let params_int = Simple {to_string = string_of_int; of_string = int_of_string}
  let params_string = let id x = x in Simple {to_string = id; of_string = id}
end

module T = struct
  type nonrec 'a t = 'a iso
  type nonrec 'a attribute = 'a attribute
end

let apply_iso t f b =
  match t with
  | Simple _ -> failwith "apply_iso"
  | Complex {fwd; bwd} ->
    Complex {
      fwd = (fun x -> fwd (f x));
      bwd = fun y -> b (bwd y)
    }

open Ppx_type_directed_value_runtime.Type_directed

let rec of_record : type a len. (a, len) Record(T).t -> a T.t = fun r ->
  let fwd v x =
    match v.Key.value with
    | Simple {to_string; _} ->
      let name =
        match v.attribute with
        | Some (Key k) -> k
        | None -> v.name
      in
      [name, to_string x]
    | Complex {fwd; _} -> fwd x
  in
  let bwd l v =
    match v.Key.value with
    | Simple {of_string; _} ->
      let name =
        match v.attribute with
        | Some (Key k) -> k
        | None -> v.name
      in
      of_string (List.assoc name l)
    | Complex {bwd; _} -> bwd l
  in
  match r with
  | [ v ] ->
    Complex {
      fwd = (fun (x, ()) -> fwd v x);
      bwd = (fun l -> bwd l v, ())
    }
  | v :: (_ :: _ as tl) ->
    match of_record tl with
    | Simple _ -> failwith "of_record got Simple"
    | Complex {fwd = fwd'; bwd = bwd'} ->
      Complex {
        fwd = (fun (x, y) -> fwd v x @ fwd' y);
        bwd = fun l -> bwd l v, bwd' l
      }

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
