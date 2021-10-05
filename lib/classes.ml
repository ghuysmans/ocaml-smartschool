open Protocol_conv_json

module Int = struct
  type t = int

  let of_json_exn = function
    | `String s -> int_of_string s
    | _ -> failwith "of_json_exn"

  let to_json i = `String (string_of_int i)
end

type class_ = {
  id: Int.t;
  code: string;
  name: string;
  desc: string;
  official: bool [@key "isOfficial"];
  (*
  isOfficial = false
  isClass = true
  *)
  visible: bool [@key "isVisible"];
  untis: string;
} [@@deriving protocol ~driver:(module Json)]

type t = class_ list [@@deriving protocol ~driver:(module Json)]
