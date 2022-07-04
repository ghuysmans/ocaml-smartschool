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

let params l = ["", String.concat "," (List.map string_of_int l)]
