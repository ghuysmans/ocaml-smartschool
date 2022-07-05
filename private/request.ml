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
      params = Params.to_xml_light {l = f p};
    }
  in
  match c with
  | Assignments x ->
    f "agenda" "show form" Agenda.Assignment.Command.params x
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

type t = {
  l: command list [@key "command"];
} [@@deriving to_protocol ~driver:(module Xml_light)]

let to_xml_light t =
  match to_xml_light t with
  | Xml.Element (_, _, ch) -> Xml.Element ("request", [], ch)
  | _ -> failwith "Request.to_xml_light"
