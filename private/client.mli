module Make : functor (C : Cohttp_lwt.S.Client) ->
  sig
    type context

    val hijack : host:string -> user_agent:string -> C.ctx -> context

    module Agenda :
      sig
        type lesson = Agenda.Query.Action_data.lesson
        type assignment = Agenda.Assignment.t
        type filter = Agenda.filter
        type command

        val timestamp : y:int -> m:int -> d:int -> int

        val call : context -> command list -> Response.action list Lwt.t

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
        type box = Postboxes.Box.t
        type item = Postboxes.Query.Action_data.message
        type message = Postboxes.Fetch_message.Action_data.message
        type attachment = Postboxes.Query_attachments.Action_data.attachment
        type command

        val box_of_string : string -> box

        val call : context -> command list -> Response.action list Lwt.t

        val messages : context -> box -> item list Lwt.t

        val message : context -> box -> int -> message Lwt.t

        val attachments : context -> box -> int -> attachment list Lwt.t

        val attachment_uri : context -> attachment -> Uri.t

        val attachment : context -> ?fn:string -> attachment -> string Lwt.t

        val delete : context -> box -> int -> unit Lwt.t
      end
  end
