open Smartschool_private.Client

let () = Lwt_main.run (
  let get_ctx () = Net_config.(hijack ~host ~cookie ~user_agent) in
  let get_dates d m y =
    let d, m, y = int_of_string d, int_of_string m, int_of_string y in
    let start = Agenda.timestamp ~y ~m ~d in
    start, start + 60 * 60 * 24 * 5
  in
  match Sys.argv with
  | [| _; "query"; d; m; y |] ->
    let start, end_ = get_dates d m y in
    let ctx = get_ctx () in
    let%lwt l = Agenda.lessons_with_assignments ctx start end_ in
    l |> List.iter (fun ((x : Agenda.lesson), a) ->
      let moments = String.concat "-" (List.map string_of_int x.moments) in
      let lessons = String.concat "-" (List.map string_of_int x.lessons) in
      let courses = String.concat ", " x.courses in
      let classrooms = String.concat ", " x.classrooms in
      let s = if x.assignment_end || x.test_deadline then "*" else "" in
      Printf.printf
        "mid=%s lid=%s %s @ %s%s\n\t%s\n"
        moments lessons courses classrooms s
        x.subject;
      a |> List.iter (fun (x : Agenda.assignment) ->
        Printf.printf "\tid=%d %s (%s)\n"
          x.assignment_id x.description x.assigned_date
      )
    );
    Lwt.return ()
  | [| _; "edit"; d; m; y; mid; lid; subject; note |] ->
    let start, end_ = get_dates d m y in
    let moment_id = int_of_string mid in
    let lesson_id = int_of_string lid in
    let ctx = get_ctx () in
    let%lwt l = Agenda.lessons ctx start end_ in
    l |> Lwt_list.iter_s (fun (x : Agenda.lesson) ->
      if List.mem moment_id x.moments && List.mem lesson_id x.lessons then
        let%lwt () = Agenda.edit ctx ~start ~end_ ~moment_id ~lesson_id ~note ~subject x in
        Lwt_io.printl "done"
      else
        Lwt.return ()
    )
  | _ ->
    Printf.eprintf
      "usage:\t%s query d m y | edit d m y mid lid subject note\n"
      Sys.argv.(0);
    exit 1
)
