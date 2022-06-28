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

module Edit = struct
  module Request = struct
    type text = {
      text: string;
      moment_id: int [@key "momentid"];
    } [@@deriving to_protocol ~driver:(module Xml_light)]

    type subjects = {
      subject: text;
    } [@@deriving to_protocol ~driver:(module Xml_light)]

    type notes = {
      note: text;
    } [@@deriving to_protocol ~driver:(module Xml_light)]

    type end_moment = {
      class_id: int;
      moment_id: int;
    }

    type end_moments = end_moment list

    let end_moments_to_xml_light l =
      Xml.Element ("endmoments", [],
        l |> List.map (fun {class_id; moment_id} ->
          Xml.(Element (
            "endmoment",
            ["classid", string_of_int class_id; "method", "list"],
            [PCData (string_of_int moment_id)]
          ))
        )
      )

    (* FIXME userid, selected attributes *)
    type student = string [@@deriving to_protocol ~driver:(module Xml_light)]

    type class_ = {
      l: student list [@key "student"];
    } [@@deriving to_protocol ~driver:(module Xml_light)]

    type students = {
      l: class_ list [@key "class"];
    } [@@deriving to_protocol ~driver:(module Xml_light)]

    type assignment = {
      lesson_id: int [@key "lessonid"] [@default 0];
      assignment_id: int [@key "assignmentid"];
      start_moment: int [@key "startmoment"];
      end_moments: end_moments [@key "endmoments"];
      typ: int [@key "type"];
      description: string;
      status: int;
      assigned_date: string [@key "assigneddate"];
      students: students;
    } [@@deriving to_protocol ~driver:(module Xml_light)]

    type assignments = assignment list

    let assignments_to_xml_light l =
      Xml.Element ("assignments", [],
        l |> List.map (fun x ->
          match assignment_to_xml_light x with
          | Xml.Element (_, _, ch) ->
            Xml.Element ("assignment", ["lessonid", string_of_int x.lesson_id], ch)
          | _ -> failwith "assignments_to_xml_light"
        )
      )

    (* FIXME *)
    type nil = string [@@deriving to_protocol ~driver:(module Xml_light)]

    type t = {
      moment_id: int [@key "momentid"];
      subjects: subjects;
      notes: notes;
      assignments: assignments;
      unique_ids: nil [@key "uniqueids"];
    } [@@deriving to_protocol ~driver:(module Xml_light)]

    let make ?class_ ?(assignments=[]) ~start ~end_ ~moment_id ~notes ~color ~lesson_id subject =
      let ft, fi =
        (* FIXME don't duplicate *)
        match class_ with
        | None -> "false", ""
        | Some c -> "Class", string_of_int c
      in
      {Request.command = {
        subsystem = "agenda";
        action = "save form";
        params = {l = [
          "startDateTimestamp", string_of_int start;
          "endDateTimestamp", string_of_int end_;
          "xmlString", Xml.to_string @@ to_xml_light @@ {
            moment_id;
            subjects = {
              subject = {
                text = subject;
                moment_id;
              }
            };
            notes = {
              note = {
                text = notes;
                moment_id;
              }
            };
            assignments;
            unique_ids = "";
          };
          "gridType", "2";
          "filterType", ft;
          "filterID", fi;
          "filterMemory", "0";
          "copySubject", "0";
          "copySubjectType", "hour";
          "copyMaterial", "0";
          "copyMaterialType", "hour";
          "copyReservations", "0";
          "copyReservationsType", "hour";
          "copyNote", "0";
          "copyNoteType", "hour";
          "copyYp", "0";
          "copyYpType", "hour";
          "color", color;
          "componentsHidden", "";
          "lessonID", string_of_int lesson_id;
        ]}
      }}
  end
end
