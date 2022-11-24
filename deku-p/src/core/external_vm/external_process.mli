exception Process_closed of Unix.process_status
exception Failed_to_parse_json of string * Data_encoding.Json.t

(* FIXME: I autogenerated this. Prune later *)
val raise : exn -> 'a
val read_all : Unix.file_descr -> int -> bytes
val write_all : Unix.file_descr -> bytes -> unit
val send_to_vm : fd:Unix.file_descr -> Data_encoding.Json.t -> unit
val read_from_vm : fd:Unix.file_descr -> Data_encoding.Json.t

type ('a, 'b) t = {
  send : 'a -> unit;
  receive : unit -> 'b;
  close : unit -> unit;
}

val open_pipes :
  named_pipe_path:string ->
  of_yojson:(Data_encoding.Json.t -> 'a) ->
  to_yojson:('b -> Data_encoding.Json.t) ->
  is_chain:bool ->
  ('b, 'a) t

val open_vm_pipes :
  named_pipe_path:string ->
  of_yojson:(Data_encoding.Json.t -> 'a) ->
  to_yojson:('b -> Data_encoding.Json.t) ->
  ('b, 'a) t

val open_chain_pipes :
  named_pipe_path:string ->
  of_yojson:(Data_encoding.Json.t -> 'a) ->
  to_yojson:('b -> Data_encoding.Json.t) ->
  ('b, 'a) t
