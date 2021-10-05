open Protocol_conv_xml

module Teacher = struct
  type t = {
    firstname: string;
    lastname: string;
    username: string;
    (* number: int; (* empty, why? *) *)
  } [@@deriving protocol ~driver:(module Xml_light)]
end

module Student_group = struct
  type typ =
    | Class [@key "K"]
    | Group [@key "G"]
    [@@deriving protocol ~driver:(module Xml_light)]

  type t = {
    name: string;
    description: string;
    typ: typ [@key "type"];
  } [@@deriving protocol ~driver:(module Xml_light)]
end

module Course = struct
  type co_teachers = {
    l: Teacher.t list [@key "coTeacher"];
  } [@@deriving protocol ~driver:(module Xml_light)]

  type student_groups = {
    l: Student_group.t list [@key "studentGroup"];
  } [@@deriving protocol ~driver:(module Xml_light)]

  type t = {
    name: string;
    description: string;
    active: int;
    main_teacher: Teacher.t [@key "mainTeacher"];
    co_teachers: co_teachers [@key "coTeachers"];
    student_groups: student_groups [@key "studentGroups"];
  } [@@deriving protocol ~driver:(module Xml_light)]
end

type t = Course.t list [@@deriving protocol ~driver:(module Xml_light)]
