open Protocol_conv_xml

type param = string * string
type ('a, 'b) iso = {fwd: 'a -> 'b; bwd: 'b -> 'a}
type 'a derived =
  | Simple of ('a, string) iso
  | Complex of ('a, param list) iso
type 'a attribute = Key of string

let map t b f =
  let map {fwd; bwd} =
    {
      fwd = (fun x -> fwd (f x));
      bwd = fun y -> b (bwd y)
    }
  in
  match t with
  | Simple i -> Simple (map i)
  | Complex i -> Complex (map i)

module D = struct
  let key x = Key x

  let params_bool =
    Simple {
      fwd = (fun x -> if x then "true" else "false");
      bwd = (function
        | "true" -> true
        | "false" -> false
        | _ -> failwith "params_bool"
      )
    }

  let params_int = Simple {fwd = string_of_int; bwd = int_of_string}
  let params_string = let id x = x in Simple {fwd = id; bwd = id}

  let add_const x l =
    match x with
    | Simple _ -> failwith "add_const"
    | Complex {fwd; bwd} -> Complex {bwd; fwd = fun x -> fwd x @ l}

  let map_xml f g =
    let fwd x = Xml.to_string (f x) in
    let bwd y = g (Xml.parse_string y) in
    Simple {fwd; bwd}
end

module T = struct
  type nonrec 'a t = 'a derived
  type nonrec 'a attribute = 'a attribute
end

let apply_iso = map

open Ppx_type_directed_value_runtime.Type_directed

let rec of_record : type a len. (a, len) Record(T).t -> a T.t = fun r ->
  let fwd v x =
    match v.Key.value with
    | Simple {fwd; _} ->
      let name =
        match v.attribute with
        | Some (Key k) -> k
        | None -> v.name
      in
      [name, fwd x]
    | Complex {fwd; _} -> fwd x
  in
  let bwd l v =
    match v.Key.value with
    | Simple {bwd; _} ->
      let name =
        match v.attribute with
        | Some (Key k) -> k
        | None -> v.name
      in
      bwd (List.assoc name l)
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
    | Simple _ -> failwith "of_record got simple"
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
