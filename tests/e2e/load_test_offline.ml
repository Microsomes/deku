open Cmdliner
open Load_test_helpers
open Helpers

(**
This load-test measures the TPS capacity of Deku. To run it do:

./sandbox.sh tear-down
./sandbox.sh setup
./sandbox.sh deploy-dummy-ticket
./sandbox.sh start

and in a new terminal do

./sandbox.sh load-test

To test TPS, we create a large amount of "unit" transactions which transfer a
dummy ticket from one hardcoded acount to another. These tickets do not
contain data, and so performance will be different if change the tickets to a
more representative kind.

The load-test then spams these transactions to the
nodes. We select the hash of the last generated transaction, and query the
Deku state until we can see that the operation corresponding to our hash has
appeared in Deku's applied blocks. At this point we select the first block
where that hash appears (final block), select the block where we started
spamming transactions (initial block), grab the sizes of the applied blocks
in this range, and calculate the time difference between the initial and
final block.

Let: 
- I be the initial block 
- F be the final block 
- It be the timestamp of the initial block
- Ft be the timestamp of the final block
- L be the length of the applied blocks

Then TPS(I, F) = L / (Ft - It)
Note that the load-test calculates the TPS offline using Deku data after it
has been recorded, as opposed to Prometheus which calculates TPS on the fly
by observing variables in Deku.

There are 3 parameters we can pass to the load-test: 
- Batch count
- Batch size
- Wait time

The amount of transactions we pass during the load-test is 
(Batch count * Batch size). The wait time is a tuning parameter that determines how long we wait before querying Deku about the applied blocks. Currently we have no way to provide load test parameters of the form e.g. send 1k transactions a second for 5 minutes. Instead our current tests look like e.g. send 50k transactions and tell me when they've all been processed.

~~TODO: Refactor spam out~~
~~TODO: Remove "saturate" extra params~~
~~TODO: Clean up printfs~~
~~TODO: Create single data for delay and batch sizes~~ 
TODO: Rebase on d4hines/develop
~~TODO: Make spam_transactions return nothing && track last transaction~~ 
TODO: bind all transaction promises together and return once last one finished
TODO: Create more representative operations to track (ops vs transacts)
TODO: Tracking function runtime so we can run tests based on load rate instead of transaction count
TODO: Do these numbers agree with prometheus


TODO: Tracking function runtime so we can run tests based on load rate instead of transaction count
TODO: Do these numbers agree with prometheus *)

let delay = 10

let batch_count = 50

let batch_size = 50

let spam_transactions ~ticketer ~n () =
  let validator_uri = get_random_validator_uri () in
  let%await block_level = get_current_block_level () in
  let ticket = make_ticket ticketer in
  let transactions =
    List.init n (fun _ ->
        make_transaction ~block_level ~ticket ~sender:alice_wallet
          ~recipient:bob_wallet ~amount:1) in
  let%await _ =
    Network.request_user_operations_gossip
      { user_operations = transactions }
      validator_uri in
  await ()

let rec get_last_block_height hash previous_level =
  let open Network in
  let uri = get_random_validator_uri () in
  let%await reply, new_level =
    request_block_by_user_operation_included
      { operation_hash = hash; previous_level }
      uri in
  match reply with
  | Some block_height -> await block_height
  | None ->
    let rec wait_until_good () =
      (* This parameter controls how often we query Deku to see if
          the final transaction has been included in applied blocks.
          This number is high to prevent DDosing the nodes.
      *)
      Unix.sleep delay;
      let%await current_level = get_current_block_level () in
      if await current_level = await new_level then
        wait_until_good ()
      else
        get_last_block_height hash new_level in
    wait_until_good ()

let process_transactions timestamps_and_blocks =
  let timestamps, blocks =
    List.fold_left
      (fun acc bt ->
        let timestamps = bt.Network.Block_by_level_spec.timestamp :: fst acc in
        let blocks = bt.Network.Block_by_level_spec.block :: snd acc in
        (timestamps, blocks))
      ([], []) timestamps_and_blocks in
  let final_time = List.hd timestamps in
  let first_time = List.hd @@ List.rev timestamps in
  let time_elapsed = final_time -. first_time in
  let total_transactions =
    List.fold_left
      (fun acc block ->
        let user_operations = Protocol.Block.parse_user_operations block in
        let transactions_per_block = List.length user_operations in
        let i = acc + transactions_per_block in
        Format.eprintf "transactions per block, block height: %Ld, %i\n%!"
          block.block_height transactions_per_block;
        i)
      0 (List.rev blocks) in
  (total_transactions, time_elapsed)

let get_block_response_by_level level =
  let validator_uri = get_random_validator_uri () in
  let%await response =
    Network.request_block_by_level { level = Int64.of_int level } validator_uri
  in
  let%await _ =
    match response with
    | Some _ -> await ()
    | None ->
      failwith
        (Printf.sprintf "get_block_response_by_level failed with level %d%!"
           level) in
  await (Option.get response)

(*
TODO: Pass batch info as a parameter to load-test-transactions so we can run it over parameter space and see if we get different outputs. 
*)
let load_test_transactions ticketer =
  let%await starting_block_level = get_current_block_level () in
  let transactions =
    List.init batch_count (fun _ ->
        spam_transactions ~ticketer ~n:batch_size ()) in
  let%await block_level = get_current_block_level () in
  let transaction =
    let ticket = make_ticket ticketer in
    make_transaction ~block_level ~ticket ~sender:alice_wallet
      ~recipient:bob_wallet ~amount:1 in
  let final_transaction =
    Network.request_user_operations_gossip
      { user_operations = [transaction] }
      (get_random_validator_uri ()) in
  let%await _ = Lwt.all (final_transaction :: transactions) in
  let transaction_hash =
    (fun op -> op.Protocol.Operation.Core_user.hash) transaction in
  let%await final_block_level =
    get_last_block_height transaction_hash starting_block_level in
  let tps_period =
    Int64.to_int (Int64.sub final_block_level starting_block_level) in
  let starting_point = Int64.to_int starting_block_level in
  (* TODO: We should be able to turn this into a single list init*)
  let%await timestamps_and_blocks =
    List.init (tps_period + 1) (fun i -> i + starting_point)
    |> Lwt_list.map_s (fun level -> get_block_response_by_level level) in
  let total_transactions_found, time_elapsed =
    process_transactions timestamps_and_blocks in
  let total_transactions_expected = (batch_count * batch_size) + 1 in
  Format.eprintf "total transactions found: %i\n%!" total_transactions_found;
  Format.eprintf "total transactions expected: %i\n%!"
    total_transactions_expected;
  assert (total_transactions_found = total_transactions_expected);
  let tps = Float.of_int total_transactions_found /. time_elapsed in
  Format.eprintf "TPS: %f\n%!" tps;
  await ()

let load_test_transactions ticketer =
  load_test_transactions ticketer |> Lwt_main.run

let args =
  let open Arg in
  let ticketer =
    let docv = "ticketer" in
    let doc =
      "Tezos address of the contract issuing the ticket (e.g. \
       KT1Ec5eb7WZNuqWDUdcFM1c2XcmwjWsJrrxb)" in
    required & pos 1 (some string) None & info [] ~doc ~docv in
  let open Term in
  const load_test_transactions $ ticketer

let _ = Cmd.eval @@ Cmd.v (Cmd.info "load-test-offline") args
