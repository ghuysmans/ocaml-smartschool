open Protocol_conv_xml
open Params.D

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
  module Action_data = struct
    type assignment_tab = {
      l: l [@key "assignments"];
    } [@@deriving of_protocol ~driver:(module Xml_light)]

    type form = {
      a: assignment_tab [@key "assignmenttab"];
    } [@@deriving of_protocol ~driver:(module Xml_light)]

    type t = {
      form: form;
    } [@@deriving of_protocol ~driver:(module Xml_light)]
  end

  module Command = struct
    type t = {
      moment_id: int [@params key "momentID"];
      lesson_id: int [@params key "lessonID"];
      class_ids: Ids.t [@params key "classIDs"];
    } [@@deriving params]

    let params =
      add_const params [
        "filterType", "false";
        "filterID", "false";
        "dateID", "";
        "assignmentIDs", "";
        "activityID", "0";
        "gridType", "2";
        "tab_to_show", "0";
        "show_assignment", "0";
      ]
  end
end

type filter =
  | Class of int
  | Teacher of int

type filter_opt = filter option

let params_filter_opt =
  let fwd x =
    let ft, fi =
      match x with
      | None -> "false", "false"
      | Some (Class c) -> "Class", string_of_int c
      | Some (Teacher t) -> "Teacher", string_of_int t
    in
    ["filterType", ft; "filterID", fi]
  in
  let bwd l =
    match List.assoc "filterType" l, List.assoc "filterID" l with
    | "false", "false" -> None
    | "Class", x -> Some (Class (int_of_string x))
    | "Teacher", x -> Some (Teacher (int_of_string x))
    | _ -> failwith "filterType"
  in
  Params.Complex {fwd; bwd}

module Query = struct
  module Action_data = struct
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

    type t = {
      content: content;
    } [@@deriving protocol ~driver:(module Xml_light)]
  end

  module Command = struct
    type t = {
      start: int [@params key "startDateTimestamp"];
      end_: int [@params key "endDateTimestamp"];
      filter: filter_opt;
    } [@@deriving params]

    let params =
      add_const params [
        "gridType", "2";
        "classID", "0";
        "endDateTimestampOld", "1655533390"; (* FIXME? *)
        "forcedTeacher", "0";
        "forcedClass", "0";
        "forcedClassroom", "0";
        "assignmentTypeID", "1";
      ]
  end
end

module Edit = struct
  module Command = struct
    type text = {
      text: string;
      moment_id: int [@key "momentid"];
    } [@@deriving protocol ~driver:(module Xml_light)]

    type subjects = {
      subject: text;
    } [@@deriving protocol ~driver:(module Xml_light)]

    type notes = {
      note: text;
    } [@@deriving protocol ~driver:(module Xml_light)]

    (* FIXME *)
    type nil = string [@@deriving protocol ~driver:(module Xml_light)]

    type xml = {
      moment_id: int [@key "momentid"];
      subjects: subjects;
      notes: notes;
      assignments: Assignment.l;
      unique_ids: nil [@key "uniqueids"];
    } [@@deriving protocol ~driver:(module Xml_light)]

    let xml_to_xml_light t =
      match xml_to_xml_light t with
      | Xml.Element (_, _, ch) -> Xml.Element ("xml", [], ch)
      | _ -> failwith "Edit.Request.xml_to_xml_light"

    let params_xml = map_xml xml_to_xml_light xml_of_xml_light_exn

    type t = {
      start: int [@params key "startDateTimestamp"];
      end_: int [@params key "endDateTimestamp"];
      xml: xml [@params key "xmlString"];
      lesson_id: int [@params key "lessonID"];
      color: string;
      filter: filter_opt;
    } [@@deriving params]

    let params =
      add_const params [
        "gridType", "2";
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
        "componentsHidden", "";
      ]
  end
end

module Stream_file = struct
  module Notification_data = struct
    type content = {
      random: string;
      title: string;
      filesize: int;
      extension: string;
    } [@@deriving of_protocol ~driver:(module Xml_light)]

    type t = {
      content: content;
    } [@@deriving of_protocol ~driver:(module Xml_light)]
  end

  module Request = struct
    type t = {
      extension: string;
      random: string;
      title: string;
      file_size: int [@key "filesize"];
    } [@@deriving params]

    let params =
      add_const params [
        "module", "Agenda";
        "file", "stream";
        "function", "streamFile";
      ]
  end
end

module Print = struct
  module Teacher_list = struct
    module Command = struct
      (* FIXME *)
      type assignment_type = int [@@deriving protocol ~driver:(module Xml_light)]
      let all_assignment_types = [1; 5; 6; 8; 10; 12]

      type items = {
        l: assignment_type list [@key "item"];
      } [@@deriving protocol ~driver:(module Xml_light)]

      type xml = {
        items: items;
      } [@@deriving protocol ~driver:(module Xml_light)]

      let xml_to_xml_light t =
        match xml_to_xml_light t with
        | Xml.Element (_, _, ch) -> Xml.Element ("xml", [], ch)
        | _ -> failwith "Print.Teacher_list.Request.xml_to_xml_light"

      let params_xml = map_xml xml_to_xml_light xml_of_xml_light_exn

      type t = {
        start: int [@params key "startDateTimestamp"];
        end_: int [@params key "endDateTimestamp"];
        assignment_types: xml [@params key "assignmentTypesXml"];
        subject: Binary.t [@params key "showSubject"];
        room: Binary.t [@params key "showClassroom"];
        start_moment: Binary.t [@params key "showStartMoments"];
        note: Binary.t [@params key "showNote"];
        daily: Binary.t [@params key "showDaynewpage"];
        color: Binary.t [@params key "showColor"];
        empty: Binary.t [@params key "showEmpty"];
        filter: filter_opt;
      } [@@deriving params]
    end
  end
end
