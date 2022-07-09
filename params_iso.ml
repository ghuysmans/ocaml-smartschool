open Protocol_conv_xml

type param = string * string
type ('a, 'b) iso = {fwd: 'a -> 'b; bwd: 'b -> 'a}

type component = string
type formatter = component list -> string
let dotted_path l = String.concat "." (List.rev l)

type 'a t = formatter -> component list -> ('a, param list) iso
type 'a attribute = Key of string

let simple fwd bwd fmt rpath =
  {
    fwd = (fun x -> [fmt rpath, fwd x]);
    bwd = fun l -> bwd (List.assoc (fmt rpath) l)
  }

module D = struct
  let key x = Key x

  let params_bool =
    simple
      (fun x -> if x then "true" else "false")
      (function
        | "true" -> true
        | "false" -> false
        | _ -> failwith "params_bool")

  let params_int = simple string_of_int int_of_string
  let params_string = let id x = x in simple id id

  let add_const f l fmt rpath =
    let {fwd; bwd} = f fmt rpath in
    {bwd; fwd = fun x -> fwd x @ l}

  (*
  let map_xml f g =
    let fwd x = Xml.to_string (f x) in
    let bwd y = g (Xml.parse_string y) in
    Simple {fwd; bwd}
  *)

  let params_option f fmt rpath =
    let {fwd; bwd} = f fmt rpath in
    let fwd x = Option.to_list x |> List.map (fun s -> fmt rpath, fwd s) in
    let bwd l = Option.map 
end

module M = Ppx_type_directed_value_runtime.Converters.Of_simple_with_key (struct
  type 'a t = formatter -> component list -> ('a, param list) iso
  type nonrec 'a attribute = 'a attribute

  let apply_iso t g f fmt rpath =
    let {fwd; bwd} = t fmt rpath in
    {
      fwd = (fun x -> fwd (f x));
      bwd = fun y -> g (bwd y)
    }

  open Ppx_type_directed_value_runtime.Type_directed

  let name v =
    match v.Key.attribute with
    | Some (Key k) -> k
    | None -> v.name

  (* FIXME remove one abstract type? *)
  let both_key k t fmt rpath =
    let {fwd; bwd} = k.Key.value fmt (name k :: rpath) in
    let {fwd = fwd'; bwd = bwd'} = t fmt rpath in
    {
      fwd = (fun (x, y) -> fwd x @ fwd' y);
      bwd = fun l -> bwd l, bwd' l
    }

  let either_key k t fmt rpath =
    let {fwd; bwd} = k.Key.value fmt (name k :: rpath) in
    let {fwd = fwd'; bwd = bwd'} = t fmt rpath in
    {
      fwd = (function
        | Base.Either.First x -> fwd x
        | Second y -> fwd' y
      );
      bwd = fun l ->
        if List.assoc (fmt (name k :: rpath)) l = name k then
          Base.Either.First (bwd l)
        else
          Second (bwd' l)
    }

  let both f g fmt rpath =
    let {fwd; bwd} = f fmt rpath in
    let {fwd = fwd'; bwd = bwd'} = g fmt rpath in
    {
      fwd = (fun (x, y) -> fwd x @ fwd' y);
      bwd = fun l -> bwd l, bwd' l
    }

  let unit _ _ =
    {
      fwd = (fun () -> []);
      bwd = fun _ -> ()
    }

  let nothing _ _ =
    {
      fwd = (function (_ : Base.Nothing.t) -> .);
      bwd = fun _ -> failwith "nothing"
    }
end)

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
