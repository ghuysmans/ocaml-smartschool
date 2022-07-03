module Make : functor (C : Cohttp_lwt.S.Client) ->
  sig
    type context

    val hijack : host:string -> user_agent:string -> C.ctx -> context

    module Agenda :
      sig
        type lesson = Agenda.Query.Response_data.lesson
        type assignment = Agenda.Assignment.t
        type filter = Agenda.filter

        val timestamp : y:int -> m:int -> d:int -> int

        val lessons : context -> ?filter:filter -> int -> int -> lesson list Lwt.t

        val assignments : context -> lesson_id:int -> int -> assignment list Lwt.t

        val lessons_with_assignments :
          context ->
          ?filter:filter ->
          int ->
          int ->
          (lesson * assignment list) list Lwt.t

        val edit :
          context ->
          start:int ->
          end_:int ->
          moment_id:int ->
          lesson_id:int ->
          ?color:string ->
          ?note:string ->
          ?subject:string ->
          lesson -> unit Lwt.t

        module Print :
          sig
            val teacher_list :
              ?teacher:int ->
              ?fn:string ->
              context ->
              start:int ->
              end_:int ->
              subject:bool ->
              room:bool ->
              start_moment:bool ->
              note:bool ->
              daily:bool -> color:bool -> empty:bool -> string Lwt.t
          end
      end

    module Postboxes :
      sig
        type box_type = Postboxes.box_type
        type item = Postboxes.Query.Response_data.message
        type message = Postboxes.Fetch_message.Response_data.message
        type attachment = Postboxes.Query_attachments.Response_data.attachment

        val messages : context -> string -> item list Lwt.t

        val message : context -> string -> int -> message Lwt.t

        val attachments : context -> string -> int -> attachment list Lwt.t

        val attachment_uri : context -> attachment -> Uri.t

        val delete : context -> string -> int -> unit Lwt.t
      end
  end
