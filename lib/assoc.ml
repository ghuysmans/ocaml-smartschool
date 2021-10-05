(* FIXME? *)
type 'a t = (string * 'a) list

let of_yojson f = function
  | `Assoc l ->
    List.fold_right (fun (k, x) acc ->
      match f x, acc with
      | Ok h, Ok t -> Ok ((k, h) :: t)
      | _, Error e -> Error e
      | Error e, _ -> Error e
    ) l (Ok [])
  | _ -> Error "Assoc.of_yojson"

let to_yojson f l =
  `Assoc (List.map (fun (k, v) -> k, f v) l)
