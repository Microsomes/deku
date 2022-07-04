open Load_test_helpers
open Helpers
open Cmdliner

let spam_transactions ~ticketer ~n () =
  let validator_uri = get_random_validator_uri () in
  let%await block_level = get_current_block_level () in
  let ticket = make_ticket ticketer in
  let transactions =
    List.init n (fun _ ->
        make_transaction ~block_level ~ticket ~sender:alice_wallet
          ~recipient:bob_wallet ~amount:1) in
  Format.eprintf "Total transactions: %d\n%!" (List.length transactions);
  let%await _ =
    Network.request_user_operations_gossip
      { user_operations = transactions }
      validator_uri in
  Lwt.return transactions

let rec spam ~ticketer =
  let n = 100 in
  let rounds = 2 in
  let%await _ =
    Lwt_list.iter_p Fun.id
    @@ List.init rounds (fun _ ->
           let%await _ = spam_transactions ~ticketer ~n () in
           await ()) in
  let%await () = Lwt_unix.sleep 1.0 in
  spam ~ticketer

let load_test_transactions ticketer =
  let%await starting_block_level = get_current_block_level () in
  Format.printf "Starting block level: %Li\n%!" starting_block_level;
  spam ~ticketer

let load_test_transactions ticketer =
  load_test_transactions ticketer |> Lwt_main.run

let args =
  let open Arg in
  let ticketer =
    let docv = "ticketer" in
    let doc =
      "Tezos address of the contract issuing the ticket (e.g. \
       KT1Ec5eb7WZNuqWDUdcFM1c2XcmwjWsJrrxb)" in
    required & pos 0 (some string) None & info [] ~doc ~docv in
  let open Term in
  const load_test_transactions $ ticketer

let _ = Cmd.eval @@ Cmd.v (Cmd.info "load-test-tps") args
