open Protocol_conv_xml

module Action = struct
  type t = {
    subsystem: string;
    command: string;
    data: Whatever.t;
  } [@@deriving of_protocol ~driver:(module Xml_light)]
end

type action =
  | Nil
  | Lessons of Agenda.Query.Action_data.lesson list
  | Assignments of Agenda.Assignment.t list
  | Agenda_notification of Agenda.Stream_file.Notification_data.content
  | Messages of Postboxes.Query.Action_data.message list
  | Message of Postboxes.Fetch_message.Action_data.message
  | Attachments of Postboxes.Query_attachments.Action_data.attachment list
  | Message_delete
  | Unknown of Action.t

let action_of_xml_light_exn x =
  let open Action in
  match of_xml_light_exn x with
  | {subsystem = "agenda"; command = "handle lessons"; data} ->
    let open Agenda.Query.Action_data in
    let {content = {lessons = {l}}} = of_xml_light_exn data in
    Lessons l
  | {subsystem = "agenda"; command = "handle form"; data} ->
    let open Agenda.Assignment.Action_data in
    let {form = {a = {l}}} = of_xml_light_exn data in
    Assignments l
  | {subsystem = "print"; command = "stream file"; data} ->
    let open Agenda.Stream_file.Notification_data in
    let {content} = of_xml_light_exn data in
    Agenda_notification content
  | {subsystem = "message list"; command = "rebuild"; data} ->
    let open Postboxes.Query.Action_data in
    let {messages = {l}} = of_xml_light_exn data in
    Messages l
  | {subsystem = "show message"; command = "rebuild"; data} ->
    let open Postboxes.Fetch_message.Action_data in
    let {message} = of_xml_light_exn data in
    Message message
  | {subsystem = "show attachments"; command = "rebuild"; data} ->
    let open Postboxes.Query_attachments.Action_data in
    let {attachments = {l}} = of_xml_light_exn data in
    Attachments l
  | {subsystem = "message list"; command = "finish massquick delete"; _} ->
    Message_delete
  | a -> Unknown a

type actions = {
  l: action list [@key "action"];
} [@@deriving of_protocol ~driver:(module Xml_light)]

type response = {
  status: string;
  actions: actions;
} [@@deriving of_protocol ~driver:(module Xml_light)]

type t = { (* server *)
  response: response;
} [@@deriving of_protocol ~driver:(module Xml_light)]
