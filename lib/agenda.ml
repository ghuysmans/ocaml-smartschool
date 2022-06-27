open Protocol_conv_xml

module Params = struct
  type param = string * string

  let param_to_xml_light (name, value) =
    let v = if value = "" then [] else [Xml.PCData value] in
    Xml.Element ("param", ["name", name], v)

  let param_of_xml_light_exn = function
    | Xml.(Element ("param", ["name", name], [])) -> name, ""
    | Xml.(Element ("param", ["name", name], [PCData value])) -> name, value
    | _ -> failwith "Params.of_xml_light_exn"

  type t = {
    l: param list [@key "param"];
  } [@@deriving protocol ~driver:(module Xml_light)]
end

module Request = struct
  type command = {
    subsystem: string;
    action: string;
    params: Params.t;
  } [@@deriving protocol ~driver:(module Xml_light)]

  type t = {
    command: command;
  } [@@deriving protocol ~driver:(module Xml_light)]
end

module Query = struct
  module Response = struct
    type lesson = {
      moment_id: int [@key "momentID"];
      lesson_id: int [@key "lessonID"];
      hour_id: int [@key "hourID"];
      hour_value: string [@key "hourValue"];
      hour: string;
      date: string;
      subject: string;
      course: string;
      classroom: string;
      teacher: string;
      class_: string [@key "klassen"];
      class_ids: string [@key "classIDs"]; (* FIXME split? *)
      color: string;
    } [@@deriving protocol ~driver:(module Xml_light)]

    type lessons = {
      l: lesson list [@key "lesson"];
    } [@@deriving protocol ~driver:(module Xml_light)]

    type content = {
      l: lessons [@key "lessons"];
    } [@@deriving protocol ~driver:(module Xml_light)]

    type data = {
      content: content;
    } [@@deriving protocol ~driver:(module Xml_light)]

    type action = {
      subsystem: string;
      command: string;
      data: data;
    } [@@deriving protocol ~driver:(module Xml_light)]

    type actions = {
      l: action list [@key "action"];
    } [@@deriving protocol ~driver:(module Xml_light)]

    type response = {
      status: string;
      actions: actions;
    } [@@deriving protocol ~driver:(module Xml_light)]

    type t = { (* server *)
      response: response;
    } [@@deriving protocol ~driver:(module Xml_light)]
  end

  module Request = struct
    let make ?class_ start end_ =
      let ft, fi =
        match class_ with
        | None -> "false", ""
        | Some c -> "Class", string_of_int c
      in
      {Request.command = {
        subsystem = "agenda";
        action = "get lessons";
        params = {l = [
          "startDateTimestamp", string_of_int start;
          "endDateTimestamp", string_of_int end_;
          "filterType", ft;
          "filterID", fi;
          "gridType", "2";
          "classID", "0";
          "endDateTimestampOld", "1655533390"; (* FIXME? *)
          "forcedTeacher", "0";
          "forcedClass", "0";
          "forcedClassroom", "0";
          "assignmentXml", "<assignments/>";
          "assignmentTypeID", "1";
        ]}
      }}
  end
end
