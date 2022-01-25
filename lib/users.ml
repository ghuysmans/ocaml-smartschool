open Protocol_conv_json

type group = {
  id: string; (* FIXME int? *)
  code: string;
  name: string;
  desc: string;
  is_class: bool [@key "isKlas"];
  is_official: bool [@key "isOfficial"];
} [@@deriving protocol ~driver:(module Json)]

module Gender = struct
  type t = char [@@deriving protocol ~driver:(module Json)]
  let male = 'm'
  let female = 'f'
end

module Coaccount = struct
  type t = {
    firstname: string;
    lastname: string;
    email: string;
    email_verified: bool;
    phone_number: string;
    mobile_number: string;
    typ: string;
    authenticator_app: bool;
    yubikey: bool;
    status: string;
  }

  let typ_of_json_exn = function
    | `Int 0 -> ""
    | `String x -> x
    | _ -> failwith "typ_of_json_exn"

  let nth_of_assoc a i =
    let get fmt = List.assoc (Printf.sprintf fmt i) a in
    let open Yojson.Safe.Util in
    {
      firstname = to_string @@ get "voornaam_coaccount%d";
      lastname = to_string @@ get "naam_coaccount%d";
      email = to_string @@ get "email_coaccount%d";
      email_verified = to_bool @@ get "email_coaccount%d_isVerified";
      phone_number = to_string @@ get "telefoonnummer_coaccount%d";
      mobile_number = to_string @@ get "mobielnummer_coaccount%d";
      typ = typ_of_json_exn @@ get "type_coaccount%d";
      authenticator_app = to_bool @@ get "authenticator_app_enabled%d";
      yubikey = to_bool @@ get "yubikey_enabled%d";
      status = to_string @@ get "status%d";
    }

  let of_json _ = failwith "dead"
  let of_json_exn = of_json
  let to_json = of_json
end

type user = {
  firstname: string [@key "voornaam"];
  lastname: string [@key "naam"];
  username: string [@key "gebruikersnaam"];
  status: string [@key "status"];
  gender: Gender.t [@key "geslacht"];
  birthdate: string [@key "geboortedatum"]; (* FIXME *)
  email_verified: bool [@key "isEmailVerified"];
  authenticator_app: bool [@key "isAuthenticatorAppEnabled"];
  yubikey: bool [@key "isYubikeyEnabled"];
  groups: group list;
  coaccounts: Coaccount.t array [@default [||]];
  last_login: string [@key "last_successful_login"];
  last_login_co1: string [@key "last_successful_login_coaccount1"];
  last_login_co2: string [@key "last_successful_login_coaccount2"];
} [@@deriving protocol ~driver:(module Json)]

let user_of_json_exn j =
  let u = user_of_json_exn j in
  let a = Yojson.Safe.Util.to_assoc j in
  let f = Coaccount.nth_of_assoc a in
  {u with coaccounts = Array.map f [| 1; 2; 3; 4; 5; 6 |]}

type t = user list [@@deriving protocol ~driver:(module Json)]
