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
  (* FIXME *)
  coaccount1_firstname: string [@key "voornaam_coaccount1"];
  coaccount1_lastname: string [@key "naam_coaccount1"];
  coaccount2_firstname: string [@key "voornaam_coaccount2"];
  coaccount2_lastname: string [@key "naam_coaccount2"];
} [@@deriving protocol ~driver:(module Json)]

type t = user list [@@deriving protocol ~driver:(module Json)]
