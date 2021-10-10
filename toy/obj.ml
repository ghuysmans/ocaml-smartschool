let confirm q = false (* TODO *)

let sync api db c =
  let fresh = db#dt c < db#last_update c in
  if not fresh || confirm "Écraser ?" then
    let r = ref db#first_unused in
    api#attendance db#class_ (db#dt c) |> List.iter (fun a ->
      let r =
        match db#find a#username with
        | Some x -> x
        | None ->
          match String.split_on_char '.' a#username with
          | [last; first] ->
            db#update !r ~username:a#username ~first ~last;
            let x = !r in incr r; x
          | _ -> failwith "invalid username"
      in
      if not a#is_present then db#set_absent r c
    )
