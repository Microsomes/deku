open Helpers
open Cmdliner
open Sandbox_helpers
open Cmdliner_helpers
open Tezos
open Feather

let tps_benchmark rpc_address =
  let%ok dummy_ticket_address =
    get_contract_address rpc_address "dummy_ticket" in
  let dummy_ticket_address = Address.to_string dummy_ticket_address in
  let%ok _result = process "tps-benchmark" [dummy_ticket_address] |> run_res in
  Ok ()

let term =
  let open Term in
  const tps_benchmark $ rpc_address

let info =
  let doc = "Tps benchmark based on the current ticketer tpc address" in
  Cmd.info "tps-benchmark" ~version:"%\226\128\140%VERSION%%" ~doc ~exits
