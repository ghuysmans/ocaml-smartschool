open Protocol_conv_xml
open Request.Params.D

module Box = struct
  type t =
    | Inbox [@key "inbox"]
    | Outbox [@key "outbox"]
    | Draft [@key "draft"]
    | Trash [@key "trash"]
    (* FIXME smartbox with an id (harder for Xml?) *)
    [@@deriving protocol ~driver:(module Xml_light)]

  let params x =
    let bt, bi =
      match x with
      | Inbox -> "inbox", 0
      | Outbox -> "outbox", 0
      | Draft -> "draft", 0
      | Trash -> "trash", 0
    in
    ["boxType", bt; "boxID", string_of_int bi]

  let of_string = function
    | "inbox" -> Inbox
    | "outbox" -> Outbox
    | "draft" -> Draft
    | "trash" -> Trash
    | _ -> failwith "Postboxes.Box.of_string"
end

module Query = struct
  module Action_data = struct
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
      real_box: Box.t [@key "realBox"];
    } [@@deriving of_protocol ~driver:(module Xml_light)]

    type messages = {
      l: message list [@key "message"];
    } [@@deriving of_protocol ~driver:(module Xml_light)]

    type t = {
      messages: messages;
    } [@@deriving of_protocol ~driver:(module Xml_light)]
  end

  module Command = struct
    type t = {
      box: Box.t [@params composite];
    } [@@deriving params]

    let params x =
      params x @ [
        "sortField", "date"; (* TODO *)
        "sortKey", "desc";
        "poll", "false";
        "poll_ids", "";
        "layout", "new";
      ]

    (* FIXME continue?! *)
    let make box =
      {
        Request.subsystem = "postboxes";
        action = "message list";
        params = {l = params {box}}
      }
  end
end

module Fetch_message = struct
  module Action_data = struct
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

    type t = {
      message: message;
    } [@@deriving of_protocol ~driver:(module Xml_light)]
  end

  module Command = struct
    type t = {
      box: Box.t [@params composite];
      id: int [@params key "msgID"];
    } [@@deriving params]

    let params x =
      params x @ [
        "limitList", "true";
      ]

    let make box id =
      {
        Request.subsystem = "postboxes";
        action = "show message";
        params = {l = params {box; id}}
      }
  end
end

module Query_attachments = struct
  module Action_data = struct
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

    type t = {
      attachments: attachment_list [@key "attachmentlist"];
    } [@@deriving of_protocol ~driver:(module Xml_light)]
  end

  module Command = struct
    type t = {
      box: Box.t [@params composite];
      id: int [@params key "msgID"];
    } [@@deriving params]

    let params x =
      params x @ [
        "limitList", "true";
      ]

    let make box id =
      {
        Request.subsystem = "postboxes";
        action = "attachment list";
        params = {l = params {box; id}}
      }
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

module Delete = struct
  module Action_data = struct
    type t = string [@@deriving of_protocol ~driver:(module Xml_light)]
  end

  module Command = struct
    type t = {
      box: Box.t [@params composite];
      id: int [@params key "msgID"];
    } [@@deriving params]

    let make box id =
      {
        Request.subsystem = "postboxes";
        action = "delete messages";
        params = {l = params {box; id}}
      }
  end
end
