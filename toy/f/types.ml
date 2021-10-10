type class_
type date
type student
type test

module type API = sig
  type attendance = {
    username : string;
    is_present : bool;
  }

  val attendance : class_ -> date -> attendance list
end

module type DATABASE = sig
  val class_ : unit -> class_
  val dt : test -> date
  val last_update : test -> date
  val first_unused : unit -> student
  val find : string -> student option
  val update : student -> username:string -> first:string -> last:string -> unit
  val next : student -> student
  val set_absent : student -> test -> unit
end

module type USER_INTERFACE = sig
  val confirm : string -> bool
end
