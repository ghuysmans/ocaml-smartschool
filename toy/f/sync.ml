open Types

module Make (Api : API) (Db : DATABASE) (Ui : USER_INTERFACE) = struct
  let sync c =
    let fresh = Db.dt c < Db.last_update c in
    if not fresh || Ui.confirm "Écraser ?" then
      let r = ref (Db.first_unused ()) in
      Api.attendance (Db.class_ ()) (Db.dt c) |>
      List.iter (fun (a : Api.attendance) ->
        let r =
          match Db.find a.username with
          | Some x -> x
          | None ->
            match String.split_on_char '.' a.username with
            | [last; first] ->
              Db.update !r ~username:a.username ~first ~last;
              let x = !r in r := Db.next !r; x
            | _ -> failwith "invalid username"
        in
        if not a.is_present then Db.set_absent r c
      )
end
