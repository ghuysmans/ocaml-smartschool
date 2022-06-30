open Protocol_conv_xml

type box_type =
  | Inbox [@key "inbox"]
  | Outbox [@key "outbox"]
  | Draft [@key "draft"]
  | Trash [@key "trash"]
  (* FIXME smartbox with an id *)
  [@@deriving protocol ~driver:(module Xml_light)]

module Query = struct
  module Response_data = struct
    type message = {
      id: int;
      from: string;
      from_image: string [@key "fromImage"];
      subject: string;
      date: string;
      status: int; (* FIXME *)
      attachments: int [@key "attachment"];
      unread: Binary.t;
      label: int;
      deleted: Binary.t;
      allow_reply: Binary.t [@key "allowreply"];
      allow_reply_enabled: Binary.t [@key "allowreplyenabled"];
      replied: Binary.t [@key "hasreply"];
      forwarded: Binary.t [@key "hasForward"];
      real_box: box_type [@key "realBox"];
    } [@@deriving of_protocol ~driver:(module Xml_light)]

    type messages = {
      l: message list [@key "message"];
    } [@@deriving of_protocol ~driver:(module Xml_light)]

    type data = {
      messages: messages;
    } [@@deriving of_protocol ~driver:(module Xml_light)]
  end

  module Response = Api.Response (Response_data)

  module Request = struct
    (* FIXME continue?! *)
    let make box_type =
      {Api.Request.command = {
        subsystem = "postboxes";
        action = "message list";
        params = {l = [
          "boxType", box_type; (* FIXME use the correct type *)
          "boxID", "0";
          "sortField", "date";
          "sortKey", "desc";
          "poll", "false";
          "poll_ids", "";
          "layout", "new";
        ]}
      }}
  end
end

module Fetch_message = struct
  module Response_data = struct
    type receivers = {
      l: string list [@key "to"];
    } [@@deriving of_protocol ~driver:(module Xml_light)]

    type message = {
      id: int;
      from: string;
      subject: string;
      date: string;
      body: string;
      status: int; (* FIXME *)
      attachments: int [@key "attachment"];
      unread: Binary.t;
      label: int;
      to_: receivers [@key "receivers"];
      cc: receivers [@key "ccreceivers"];
      bcc: receivers [@key "bccreceivers"];
      from_team: int [@key "fromTeam"];
      can_reply: Binary.t [@key "canReply"];
      replied: Binary.t [@key "hasReply"];
      forwarded: Binary.t [@key "hasForward"];
    } [@@deriving of_protocol ~driver:(module Xml_light)]

    type data = {
      message: message;
    } [@@deriving of_protocol ~driver:(module Xml_light)]
  end

  module Response = Api.Response (Response_data)

  module Request = struct
    let make box_type id =
      {Api.Request.command = {
        subsystem = "postboxes";
        action = "show message";
        params = {l = [
          "msgID", string_of_int id;
          "boxType", box_type; (* FIXME use the correct type *)
          "limitList", "true";
        ]}
      }}
  end
end

module Query_attachments = struct
  module Response_data = struct
    type attachment = {
      file_id: int [@key "fileID"];
      name: string;
      mime: string; (* in the user's language *)
      size: string; (* in the user's language *)
      wopi_allowed: Maybe_one.t [@key "wopiAllowed"];
      order: int;
    } [@@deriving of_protocol ~driver:(module Xml_light)]

    type attachment_list = {
      l: attachment list [@key "attachment"];
    } [@@deriving of_protocol ~driver:(module Xml_light)]

    type data = {
      attachments: attachment_list [@key "attachmentlist"];
    } [@@deriving of_protocol ~driver:(module Xml_light)]
  end

  module Response = Api.Response (Response_data)

  module Request = struct
    let make box_type id =
      {Api.Request.command = {
        subsystem = "postboxes";
        action = "attachment list";
        params = {l = [
          "msgID", string_of_int id;
          "boxType", box_type; (* FIXME use the correct type *)
          "limitList", "true";
        ]}
      }}
  end

  let uri ~host id =
    let query = [
      "module", ["Messages"];
      "file", ["download"];
      "fileID", [string_of_int id];
      "target", ["0"]
    ] in
    Uri.make ~scheme:"https" ~host ~path:"/index.php" ~query ()
end

(* FIXME file *)
