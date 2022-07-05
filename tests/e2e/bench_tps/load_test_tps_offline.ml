open Cmdliner
open Load_test_helpers
open Helpers

(*
let batch_count = 50

let batch_size = 50

let spam_transactions ~ticketer ~n () =
  let validator_uri = get_random_validator_uri () in
  let%await block_level = get_current_block_level () in
  let ticket = make_ticket ticketer in
  let transactions =
    List.init n (fun _ ->
        make_transaction ~block_level ~ticket ~sender:alice_wallet
          ~recipient:bob_wallet ~amount:1 )
  in
  let%await _ =
    Network.request_user_operations_gossip
      {user_operations= transactions}
      validator_uri
  in
  await ()

let process_transactions timestamps_and_blocks =
  let timestamps, blocks =
    List.fold_left
      (fun acc bt ->
        let timestamps = bt.Network.Block_by_level_spec.timestamp :: fst acc in
        let blocks = bt.Network.Block_by_level_spec.block :: snd acc in
        (timestamps, blocks) )
      ([], []) timestamps_and_blocks
  in
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
          block.block_height transactions_per_block ;
        i )
      0 (List.rev blocks)
  in
  (total_transactions, time_elapsed)

let load_test_transactions ticketer =
  let%await starting_block_level = get_current_block_level () in
  let transactions =
    List.init batch_count (fun _ ->
        spam_transactions ~ticketer ~n:batch_size () )
  in
  let%await block_level = get_current_block_level () in
  let transaction =
    let ticket = make_ticket ticketer in
    make_transaction ~block_level ~ticket ~sender:alice_wallet
      ~recipient:bob_wallet ~amount:1
  in
  let final_transaction =
    Network.request_user_operations_gossip
      {user_operations= [transaction]}
      (get_random_validator_uri ())
  in
  let%await _ = Lwt.all (final_transaction :: transactions) in
  let transaction_hash =
    (fun op -> op.Protocol.Operation.Core_user.hash) transaction
  in
  let%await final_block_level =
    get_last_block_height transaction_hash starting_block_level
  in
  let tps_period =
    Int64.to_int (Int64.sub final_block_level starting_block_level)
  in
  let starting_point = Int64.to_int starting_block_level in
  let%await timestamps_and_blocks =
    List.init (tps_period + 1) (fun i -> i + starting_point)
    |> Lwt_list.map_s (fun level -> get_block_response_by_level level)
  in
  let total_transactions_found, time_elapsed =
    process_transactions timestamps_and_blocks
  in
  let total_transactions_expected = (batch_count * batch_size) + 1 in
  Format.eprintf "total transactions found: %i\n%!" total_transactions_found ;
  Format.eprintf "total transactions expected: %i\n%!"
    total_transactions_expected ;
  assert (total_transactions_found = total_transactions_expected) ;
  let tps = Float.of_int total_transactions_found /. time_elapsed in
  Format.eprintf "TPS: %f\n%!" tps ;
  await ()
*)

let spam_transactions ~ticketer ~n () =
  let validator_uri = get_random_validator_uri () in
  let%await block_level = get_current_block_level () in
  let ticket = make_ticket ticketer in
  let transactions =
    List.init n (fun _ ->
        make_transaction ~block_level ~ticket ~sender:alice_wallet
          ~recipient:bob_wallet ~amount:1 )
  in
  let%await _ =
    Network.request_user_operations_gossip
      {user_operations= transactions}
      validator_uri
  in
  let transaction = transactions |> List.rev |> List.hd in
  Lwt.return transaction

let rec spam ~ticketer rounds_left ((batch_size, batch_count) as info) =
  let%await transaction =
    List.init batch_count (fun _ ->
        let transaction = spam_transactions ~ticketer ~n:batch_size () in
        transaction )
    |> List.rev |> List.hd
  in
  if rounds_left <= 0 then await transaction.Protocol.Operation.Core_user.hash
  else spam ~ticketer (rounds_left - 1) info

let process_transactions timestamps_and_blocks =
  let timestamps, blocks =
    List.fold_left
      (fun acc bt ->
        let timestamps = bt.Network.Block_by_level_spec.timestamp :: fst acc in
        let blocks = bt.Network.Block_by_level_spec.block :: snd acc in
        (timestamps, blocks) )
      ([], []) timestamps_and_blocks
  in
  let final_time = List.hd timestamps in
  Format.eprintf "final time: %.3f\n%!" final_time ;
  let first_time = List.hd @@ List.rev timestamps in
  Format.eprintf "starting time: %.3f\n%!" first_time ;
  let time_elapsed = final_time -. first_time in
  Format.eprintf "time_elapsed time: %.3f\n%!" time_elapsed ;
  let total_transactions =
    List.fold_left
      (fun acc block ->
        let user_operations = Protocol.Block.parse_user_operations block in
        let transactions_per_block = List.length user_operations in
        let i = acc + transactions_per_block in
        Format.eprintf "transactions per block, block height:%Ld, %i\n%!"
          block.block_height transactions_per_block ;
        i )
      0 (List.rev blocks)
  in
  Format.eprintf "total_transactions: %i\n%!" total_transactions ;
  let tps = Float.of_int total_transactions /. time_elapsed in
  Format.eprintf "tps_process_transactions:%.3f\n%!" tps ;
  tps

let load_test_transactions ticketer =
  let rounds = 50 in
  let batch_count = 5 in
  let batch_size = 10 in
  let%await starting_block_level = get_current_block_level () in
  Format.eprintf "Starting block level: %Li\n%!" starting_block_level ;
  let%await operation_hash = spam ~ticketer rounds (batch_size, batch_count) in
  let%await final_block_level =
    get_last_block_height operation_hash starting_block_level
  in
  Format.eprintf "Final block level: %Ld\n" final_block_level ;
  let tps_period =
    Int64.to_int (Int64.sub final_block_level starting_block_level)
  in
  Format.eprintf "tps_period: %i\n" tps_period ;
  let starting_point = Int64.to_int starting_block_level in
  Format.eprintf "starting point: %d\n%!" starting_point ;
  let%await timestamps_and_blocks =
    List.init (tps_period + 1) (fun i ->
        let block_index_of_spamming = i + starting_point in
        block_index_of_spamming )
    |> Lwt_list.map_s (fun level -> get_block_response_by_level level)
  in
  let tps = Int.of_float @@ process_transactions timestamps_and_blocks in
  Format.eprintf "TPS: %i\n%!" tps ;
  await ()

let load_test_transactions ticketer =
  load_test_transactions ticketer |> Lwt_main.run

let args =
  let open Arg in
  let ticketer =
    let docv = "ticketer" in
    let doc =
      "Tezos address of the contract issuing the ticket (e.g. \
       KT1Ec5eb7WZNuqWDUdcFM1c2XcmwjWsJrrxb)"
    in
    required & pos 0 (some string) None & info [] ~doc ~docv
  in
  let open Term in
  const load_test_transactions $ ticketer

let _ = Cmd.eval @@ Cmd.v (Cmd.info "load-test-offline") args
