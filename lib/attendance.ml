open Protocol_conv_json

module Status = struct
  (* FIXME check names *)
  type t =
    | Present [@name "|"]
    | Late [@name "R"]
    | No_reason [@name "O"]
    | Admin [@name "B"]
    | Parents [@name "P"]
    | Doctor [@name "M"]
    | Director [@name "E"]
    | Mourning [@name "D"]
    | Sport_high [@name "S"]
    | Sport [@name "F"]
    | Event [@name "V"]
    | Lone_stay [@name "J"]
    | Onboarding [@name "H"]
    | Excluded [@name "X"]
    | Unknown_reason [@name "-"]
    | Temporary [@name "I"]
    | Summoned [@name "C"]
    [@@deriving protocol ~driver:(module Json)]

  let of_json_exn = function
    | `String "" -> Present
    | yo -> of_json_exn yo

  let to_french = function
    | Present -> "Présence"
    | Late -> "En retard"
    | No_reason -> "Absence injustifiée"
    | Admin -> "Autorisée par l'Administration"
    | Parents -> "Justificatif des parents"
    | Doctor -> "Certificat médical"
    | Director -> "Justifiée par le directeur"
    | Mourning -> "Décès d'un parent ou allié"
    | Sport_high -> "Activités jeunes sportifs de haut niveau"
    | Sport -> "Activités jeunes sportifs"
    | Event -> "Événements ou activités artistiques"
    | Lone_stay -> "Séjour scolaire individuel"
    | Onboarding -> "Service d'accrochage scolaire"
    | Excluded -> "Exclusion temporaire"
    | Unknown_reason -> "Motif inconnu"
    | Temporary -> "Écartement provisoire"
    | Summoned -> "Convocation par une autorité"
end

type day = {
  am: Status.t;
  pm: Status.t;
} [@@deriving protocol ~driver:(module Json)]

type t = day Assoc.t [@@deriving protocol ~driver:(module Json)]
