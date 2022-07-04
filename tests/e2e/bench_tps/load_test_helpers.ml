open Crypto
open Node
open Helpers

open (
  struct
    include Server
  end :
    sig end )

type wallet = {key_hash: Key_hash.t; secret: Secret.t}

(* The wallets are hard-coded to make it easier to deposit the initial ticket
   and to enable expect tests on the output in the future. *)
let alice_wallet =
  { key_hash=
      Key_hash.of_string "tz1RPNjHPWuM8ryS5LDttkHdM321t85dSqaf" |> Option.get
  ; secret=
      Secret.of_string "edsk36FhrZwFVKpkdmouNmcwkAJ9XgSnE5TFHA7MqnmZ93iczDhQLK"
      |> Option.get }

let bob_wallet =
  { key_hash=
      Key_hash.of_string "tz1h1oFuYsCorjxekQ59bUe1uDGhuYvEx9ob" |> Option.get
  ; secret=
      Secret.of_string "edsk326F1xfCvHFw1LWhgtrwcm6DnFoHCmjjWX4vcWsJCbqmujJQVs"
      |> Option.get }

(* Hard-coded for now. TODO: get these dynamically, see
   https://github.com/marigold-dev/deku/pull/450 *)
let validators_uris =
  ["http://localhost:4440"; "http://localhost:4441"; "http://localhost:4442"]

let random_int v = v |> Int32.of_int |> Random.int32 |> Int32.to_int

let get_random_validator_uri () =
  let validator =
    List.nth validators_uris (random_int (List.length validators_uris))
  in
  validator |> Uri.of_string

let get_current_block_level () =
  let validator_uri = get_random_validator_uri () in
  let%await block_level = Network.request_block_level () validator_uri in
  Lwt.return block_level.level

(* Assumes that the bytes of the ticket are empty. This simplifies things quite
   a bit, since we don't have to query the contents of the ticket or serialize
   and then parse the bytes *)
let make_ticket ticketer =
  let open Tezos in
  let ticketer =
    let contract = Tezos.Contract_hash.of_string ticketer |> Option.get in
    Tezos.Address.Originated {contract; entrypoint= None}
  in
  let open Ticket_id in
  Core_deku.Ticket_id.of_tezos {ticketer; data= Bytes.empty} |> Result.get_ok

let make_transaction ~block_level ~ticket ~sender ~recipient ~amount =
  let amount = Core_deku.Amount.of_int amount in
  let transaction =
    Protocol.Operation.Core_user.sign ~secret:sender.secret
      ~nonce:(Crypto.Random.int32 Int32.max_int)
      ~block_height:block_level
      ~data:
        (Core_deku.User_operation.make ~source:sender.key_hash
           (Transaction {destination= recipient.key_hash; amount; ticket}) )
  in
  let amount2 =
    match transaction.data.initial_operation with
    | Core_deku.User_operation.Transaction t ->
        t.amount
    | _ ->
        failwith "Not a transaction\n"
  in
  assert (amount2 = amount) ;
  transaction
