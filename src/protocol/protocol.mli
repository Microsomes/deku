open Deku_crypto
open Deku_concepts

type protocol = private
  | Protocol of {
      included_operations : Included_operation_set.t;
      ledger : Ledger.t;
    }

type t = protocol

val initial : protocol

val apply :
  current:Level.t ->
  operations:(Key.t * Signature.t * Operation.t) list ->
  protocol ->
  protocol
