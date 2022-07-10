let rec of_variant_constructor : type a len. (a, len) Variant_constructor(T).t -> (a, param list) iso = fun r ->
  let fwd : type a. int -> a derived -> a -> param list = fun i v x ->
    let name = string_of_int i in
    match v with
    | Simple {fwd; _} -> [name, fwd x]
    | Complex {fwd; _} -> fwd x
    | Option {fwd; _} -> Option.to_list x |> List.map (fun s -> name, fwd s)
  in
  let bwd : type a. int -> param list -> a derived -> a = fun i l v ->
    let name = string_of_int i in
    match v with
    | Simple {bwd; _} -> bwd (List.assoc name l)
    | Complex {bwd; _} -> bwd l
    | Option {bwd; _} -> Option.map bwd (List.assoc_opt name l)
  in
  match r with
  | [] ->
    {
      fwd = (fun () -> []);
      bwd = ignore;
    }
  | v :: tl ->
    let {fwd = fwd'; bwd = bwd'} = of_variant_constructor tl in
    {
      (* FIXME how to pass i+1: change Complex? *)
      fwd = (fun (x, y) -> fwd 0 v x @ fwd' y);
      bwd = fun l -> bwd 0 l v, bwd' l
    }

let rec of_variant : type a len. (a, len) Variant(T).t -> (a, param list) iso = fun v ->
  match v with
  | {name; value; _} :: tl ->
    let fwd = function
      | Base.Either.First _ -> ["", name]
      | Second x -> (of_variant tl).fwd x
    in
    let bwd l =
      if List.assoc "" l = name then
        match value with
        | Unlabelled w -> Base.Either.First ((of_variant_constructor w).bwd l)
        | Labelled w -> First ((of_record w).bwd l)
      else
        Second ((of_variant tl).bwd l)
    in
    {fwd; bwd}
  | _ -> failwith "of_variant: empty type" (* FIXME? *)

let of_variant_constructor v = Complex (of_variant_constructor v)

let of_variant v = Complex (of_variant v)

