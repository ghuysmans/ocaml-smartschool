open Protocol_conv_xml

module Command = struct
  type t = {
    subsystem: string;
    action: string;
    params: Whatever.t;
  } [@@deriving protocol ~driver:(module Xml_light)]
end

type command =
  | Assignments of Agenda.Assignment.Command.t
  | Assignment_types
  | Lessons of Agenda.Query.Command.t
  | Lesson_edit of Agenda.Edit.Command.t
  | Teacher_print of Agenda.Print.Teacher_list.Command.t
  | Messages of Postboxes.Query.Command.t
  | Message of Postboxes.Fetch_message.Command.t
  | Attachments of Postboxes.Query_attachments.Command.t
  | Message_delete of Postboxes.Delete.Command.t
  | Unknown of Command.t

let command_to_xml_light c =
  let f subsystem action f p =
    Command.to_xml_light {
      subsystem;
      action;
      params = Params.(to_xml_light {l = to_assoc f p});
    }
  in
  match c with
  | Assignments x ->
    f "agenda" "show form" Agenda.Assignment.Command.params x
  | Assignment_types ->
    f "print" "get assignment types" Agenda.Assignment_type.Command.params ()
  | Lessons x ->
    f "agenda" "get lessons" Agenda.Query.Command.params x
  | Lesson_edit x ->
    f "agenda" "save form" Agenda.Edit.Command.params x
  | Teacher_print x ->
    f "print" "get teacher list pdf" Agenda.Print.Teacher_list.Command.params x
  | Messages x ->
    f "postboxes" "message list" Postboxes.Query.Command.params x
  | Message x ->
    f "postboxes" "show message" Postboxes.Fetch_message.Command.params x
  | Attachments x ->
    f "postboxes" "attachment list" Postboxes.Query_attachments.Command.params x
  | Message_delete x ->
    f "postboxes" "delete messages" Postboxes.Delete.Command.params x
  | Unknown c ->
    Command.to_xml_light c

let command_of_xml_light_exn x =
  let open Command in
  let f p =
    let open Params in
    match p with
    | Complex {bwd; _} -> fun y -> bwd (of_xml_light_exn y).l
    | _ -> failwith "command_of_xml_light"
  in
  match of_xml_light_exn x with
  | {subsystem = "agenda"; action = "get assignment types"; _} ->
    Assignment_types
  | {subsystem = "agenda"; action = "show form"; params} ->
    Assignments (f Agenda.Assignment.Command.params params)
  | {subsystem = "agenda"; action = "get lessons"; params} ->
    Lessons (f Agenda.Query.Command.params params)
  | {subsystem = "agenda"; action = "save form"; params} ->
    Lesson_edit (f Agenda.Edit.Command.params params)
  | {subsystem = "print"; action = "get teacher list pdf"; params} ->
    Teacher_print (f Agenda.Print.Teacher_list.Command.params params)
  | {subsystem = "postboxes"; action = "message list"; params} ->
    Messages (f Postboxes.Query.Command.params params)
  | {subsystem = "postboxes"; action = "show message"; params} ->
    Message (f Postboxes.Fetch_message.Command.params params)
  | {subsystem = "postboxes"; action = "attachment list"; params} ->
    Attachments (f Postboxes.Query_attachments.Command.params params)
  | {subsystem = "postboxes"; action = "delete messages"; params} ->
    Message_delete (f Postboxes.Delete.Command.params params)
  | c ->
    Unknown c

type t = {
  l: command list [@key "command"];
} [@@deriving protocol ~driver:(module Xml_light)]

let to_xml_light t =
  match to_xml_light t with
  | Xml.Element (_, _, ch) -> Xml.Element ("request", [], ch)
  | _ -> failwith "Request.to_xml_light"
