type t = int list

let of_xml_light_exn x =
  try
    Names.of_xml_light_exn x |>
    List.map int_of_string
  with Failure _ ->
    failwith "Ids.of_xml_light_exn"

let to_xml_light l =
  List.map string_of_int l |>
  Names.to_xml_light

let params =
  let fwd l = String.concat "," (List.map string_of_int l) in
  let bwd s = String.split_on_char ',' s |> List.map int_of_string in
  Params.Simple {fwd; bwd}
