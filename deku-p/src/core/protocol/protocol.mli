open Deku_concepts
open Deku_tezos
open Deku_ledger
open Deku_gameboy

type protocol = private
  | Protocol of {
      included_operations : Included_operation_set.t;
      included_tezos_operations : Deku_tezos.Tezos_operation_hash.Set.t;
      ledger : Ledger.t;
      vm_state : Deku_gameboy.t;
      game : Game.t;
    }

type t = protocol [@@deriving yojson]

val initial : protocol
val initial_with_vm_state : vm_state:Deku_gameboy.t -> protocol

val prepare :
  parallel:
    ((string -> Operation.Initial.t option) ->
    string list ->
    Operation.Initial.t list) ->
  payload:string list ->
  Operation.Initial.t list

val apply :
  current_level:Level.t ->
  payload:Operation.Initial.t list ->
  tezos_operations:Tezos_operation.t list ->
  protocol ->
  protocol * Receipt.t list * exn list
