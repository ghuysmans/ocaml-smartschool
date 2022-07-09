open Protocol_conv_xml

type param = string * string
type ('a, 'b) iso = {fwd: 'a -> 'b; bwd: 'b -> 'a}

type component = string
type formatter = component list -> string
let dotted_path l = String.concat "." (List.rev l)

type 'a derived =
  | Simple : ('a, string) iso -> 'a derived
  | Complex : (formatter -> component list -> ('a, param list) iso) -> 'a derived
  | Option : ('a, string) iso -> 'a option derived
type 'a attribute = Key of string

let map : type a. a derived -> (a -> 'b) -> ('b -> a) -> 'b derived = fun t b f ->
  let map {fwd; bwd} =
    {
      fwd = (fun x -> fwd (f x));
      bwd = fun y -> b (bwd y)
    }
  in
  match t with
  | Simple i -> Simple (map i)
  | Complex f -> Complex (fun fmt rpath -> map (f fmt rpath))
  | Option _ -> failwith "Params.map"

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
    | Complex f ->
      Complex (fun fmt rpath ->
        let {fwd; bwd} = f fmt rpath in
        {bwd; fwd = fun x -> fwd x @ l}
      )
    | _ -> failwith "add_const"

  let map_xml f g =
    let fwd x = Xml.to_string (f x) in
    let bwd y = g (Xml.parse_string y) in
    Simple {fwd; bwd}

  let params_option = function
    | Simple i -> Option i
    | _ -> failwith "params_option"
end

module T = struct
  type nonrec 'a t = 'a derived
  type nonrec 'a attribute = 'a attribute
end

let apply_iso = map

open Ppx_type_directed_value_runtime.Type_directed

let rec of_record : type a len. (a, len) Record(T).t -> formatter -> component list -> (a, param list) iso = fun r fmt rpath ->
  let field v =
    match v.Key.attribute with
    | Some (Key k) -> k
    | None -> v.name
  in
  let name v = fmt (field v :: rpath) in
  let fwd : type a. (a derived, _) Key.t -> a -> param list = fun v x ->
    match v.Key.value with
    | Simple {fwd; _} -> [name v, fwd x]
    | Complex f -> (f fmt (field v :: rpath)).fwd x
    | Option {fwd; _} -> Option.to_list x |> List.map (fun s -> name v, fwd s)
  in
  let bwd : type a. param list -> (a derived, _) Key.t -> a = fun l v ->
    match v.Key.value with
    | Simple {bwd; _} -> bwd (List.assoc (name v) l)
    | Complex f -> (f fmt (field v :: rpath)).bwd l
    | Option {bwd; _} -> Option.map bwd (List.assoc_opt (name v) l)
  in
  match r with
  | [ v ] ->
    {
      fwd = (fun (x, ()) -> fwd v x);
      bwd = (fun l -> bwd l v, ())
    }
  | v :: (_ :: _ as tl) ->
    let {fwd = fwd'; bwd = bwd'} = of_record tl fmt rpath in
    {
      fwd = (fun (x, y) -> fwd v x @ fwd' y);
      bwd = fun l -> bwd l v, bwd' l
    }

let of_record r = Complex (of_record r)

let param_to_xml_light (name, value) =
  let v = if value = "" then [] else [Xml.PCData value] in
  Xml.Element ("param", ["name", name], v)

let param_of_xml_light_exn = function
  | Xml.(Element ("param", ["name", name], [])) -> name, ""
  | Xml.(Element ("param", ["name", name], [PCData value])) -> name, value
  | _ -> failwith "param_of_xml_light_exn"

type t = {
  l: param list [@key "param"];
} [@@deriving protocol ~driver:(module Xml_light)]

let to_assoc ?(key_formatter=dotted_path) p x =
  match p with
  | Complex f -> (f key_formatter []).fwd x
  | _ -> failwith "Params.to_assoc"

let of_assoc ?(key_formatter=dotted_path) p x =
  match p with
  | Complex f -> (f key_formatter []).bwd x
  | _ -> failwith "Params.of_assoc"
