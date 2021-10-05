(* FIXME? *)
type 'a t = (string * 'a) list

let of_json_exn f = function
  | `Assoc l -> List.map (fun (k, v) -> k, f v) l
  | _ -> failwith "Assoc.of_json_exn"

let to_json f l =
  `Assoc (List.map (fun (k, v) -> k, f v) l)
