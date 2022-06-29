open Protocol_conv_xml

module Assignment = struct
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

  let end_moments_of_xml_light_exn = function
    | Xml.Element ("endmoments", _, l) ->
      l |> List.map (function
        | Xml.Element ("endmoment", ("classid", c) :: _, [PCData m]) ->
          {class_id = int_of_string c; moment_id = int_of_string m}
        | _ ->
          failwith "Assignment.end_moments_of_xml_light_exn"
      )
    | _ ->
      failwith "Assignment.end_moments_of_xml_light_exn"

  (* FIXME userid, selected attributes *)
  type student = string [@@deriving protocol ~driver:(module Xml_light)]

  type class_ = {
    l: student list [@key "student"];
  } [@@deriving protocol ~driver:(module Xml_light)]

  type students = {
    l: class_ list [@key "class"];
  } [@@deriving protocol ~driver:(module Xml_light)]

  type t = {
    lesson_id: string [@key "lessonid"]; (* FIXME *)
    assignment_id: int [@key "assignmentid"];
    start_moment: int [@key "startmoment"];
    end_moments: end_moments [@key "endmoments"];
    typ: int [@key "type"];
    description: string;
    status: int;
    assigned_date: string [@key "assigneddate"];
    students: students;
  } [@@deriving protocol ~driver:(module Xml_light)]

  type l = t list

  let l_to_xml_light l =
    Xml.Element ("assignments", [],
      l |> List.map (fun x ->
        match to_xml_light x with
        | Xml.Element (_, _, ch) ->
          Xml.Element ("assignment", ["lessonid", x.lesson_id], ch)
        | _ -> failwith "Assignment.l_to_xml_light"
      )
    )

  let l_of_xml_light_exn = function
    | Xml.Element ("assignments", _, l) ->
      l |> List.map (function
        | Xml.Element ("assignment", ["lessonid", lesson_id], ch) ->
          let t = of_xml_light_exn (Xml.Element ("assignment", [], ch)) in
          {t with lesson_id}
        | _ -> failwith "Assignment.l_of_xml_light_exn"
      )
    | _ ->
      failwith "Assignment.l_of_xml_light_exn"

  (* subset of form; TODO create a Lesson module? *)
  module Response_data = struct
    type assignment_tab = {
      l: l [@key "assignments"];
    } [@@deriving of_protocol ~driver:(module Xml_light)]

    type form = {
      a: assignment_tab [@key "assignmenttab"];
    } [@@deriving of_protocol ~driver:(module Xml_light)]

    type data = {
      form: form;
    } [@@deriving of_protocol ~driver:(module Xml_light)]
  end

  module Response = Api.Response (Response_data)

  module Request = struct
    let make ?(class_ids=[0]) ~lesson_id moment_id =
      {Api.Request.command = {
        subsystem = "agenda";
        action = "show form";
        params = {l = [
          "momentID", string_of_int moment_id;
          "lessonID", string_of_int lesson_id;
          "classIDs", String.concat "," (List.map string_of_int class_ids);
          "filterType", "false";
          "filterID", "false";
          "dateID", "";
          "assignmentIDs", "";
          "activityID", "0";
          "gridType", "2";
          "tab_to_show", "0";
          "show_assignment", "0";
        ]}
      }}
  end
end

module Query = struct
  module Response_data = struct
    type lesson = {
      moments: Ids.t [@key "momentID"];
      lessons: Ids.t [@key "lessonID"];
      hour_id: int [@key "hourID"];
      hour_value: string [@key "hourValue"];
      hour: string;
      date: string;
      subject: string;
      courses: Names.t [@key "courseTitle"];
      classrooms: Names.t [@key "classroomTitle"];
      teachers: Names.t [@key "teacherTitle"];
      classes_: Names.t [@key "klassen"];
      class_ids: Ids.t [@key "classIDs"];
      color: string;
      assignment_end: Binary.t [@key "assignmentEndStatus"];
      test_deadline: Binary.t [@key "testDeadlineStatus"];
      note: string;
    } [@@deriving protocol ~driver:(module Xml_light)]

    type lessons = {
      l: lesson list [@key "lesson"];
    } [@@deriving protocol ~driver:(module Xml_light)]

    type content = {
      lessons: lessons;
    } [@@deriving protocol ~driver:(module Xml_light)]

    type data = {
      content: content;
    } [@@deriving protocol ~driver:(module Xml_light)]
  end

  module Response = Api.Response (Response_data)

  module Request = struct
    let make ?class_ start end_ =
      let ft, fi =
        match class_ with
        | None -> "false", ""
        | Some c -> "Class", string_of_int c
      in
      {Api.Request.command = {
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
  module Response = Api.Response (Whatever)

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

    (* FIXME *)
    type nil = string [@@deriving to_protocol ~driver:(module Xml_light)]

    type t = {
      moment_id: int [@key "momentid"];
      subjects: subjects;
      notes: notes;
      assignments: Assignment.l;
      unique_ids: nil [@key "uniqueids"];
    } [@@deriving to_protocol ~driver:(module Xml_light)]

    let to_xml_light t =
      match to_xml_light t with
      | Xml.Element (_, _, ch) -> Xml.Element ("xml", [], ch)
      | _ -> failwith "Edit.Request.to_xml_light"

    let make ?class_ ?(assignments=[]) ~start ~end_ ~moment_id ~note ~color ~lesson_id subject =
      let ft, fi =
        (* FIXME don't duplicate *)
        match class_ with
        | None -> "false", ""
        | Some c -> "Class", string_of_int c
      in
      {Api.Request.command = {
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
                text = note;
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