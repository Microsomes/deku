open Cmdliner
open Sandbox_helpers.Cmdliner_helpers

let tear_down =
  let open Term in
  const tear_down $ nodes

let info_tear_down =
  let doc = "Stops the Tezos node and destroys the Deku state." in
  Cmd.info "tear-down" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man

(* setup *)
let setup mode validators rpc_url =
  (* FIXME: this relative path seems suspicious - does it work if you move directories? *)
  let consensus = "./src/tezos_interop/consensus.mligo" in
  let discovery = "./src/tezos_interop/discovery.mligo" in
  let secret = "edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq" in
  validators
  |> List.map (fun i -> Format.sprintf "data/%i" i)
  |> List.iter rm_dir;

  (* setup tezos-client *)
  let%assert () =
    ("the tezos node is not bootstrapped", is_node_bootstrapped rpc_url) in
  let%ok _ = tezos_client_update_config rpc_url in
  let%ok _ =
    import_secret rpc_url "myWallet" (Format.sprintf "unencrypted:%s" secret)
  in

  (* setup write indentity.json to file system *)
  let%ok identities =
    validators |> List.map (setup_identity mode) |> fold_results (Ok []) in

  (* deploy smart contracts *)
  let consensus_storage = make_consensus_storage identities in
  let discovery_storage = make_discovery_storage identities in
  let%ok consensus_address =
    deploy_contract rpc_url "consensus" consensus consensus_storage "myWallet"
  in
  let%ok discovery_address =
    deploy_contract rpc_url "discovery" discovery discovery_storage "myWallet"
  in

  (* setup tezos informations *)
  make_trusted_validator_membership_change_json identities;
  identities
  |> List.map (setup_tezos rpc_url secret consensus_address discovery_address)
  |> fold_results (Ok [])

let setup mode nodes =
  let validators = make_validators nodes in
  let rpc_url = rpc_url mode in
  setup mode validators rpc_url |> ret_res

let setup =
  let open Term in
  const setup $ mode $ nodes

let info_setup =
  let doc =
    "Does the following: it starts a Tezos sandbox network with Flextesa, then \
     it generates a new validator indentities and it deploys a new contract to \
     the Tezos sandbox configured to use these validators." in
  Cmd.info "setup" ~version:"%\226\128\140%VERSION%%" ~doc ~exits ~man

let deploy_dummy_ticket =
  let open Term in
  const deploy_dummy_ticket $ mode

let info_deploy_dummy_ticket =
  let doc =
    "Deploys a contract that forges dummy tickets and deposits to Deku." in
  Cmd.info "deploy-dummy-ticket" ~version:"%\226\128\140%VERSION%%" ~doc ~exits
    ~man

let deposit_dummy_ticket =
  let open Term in
  const deposit_dummy_ticket $ mode

let info_deposit_dummy_ticket =
  let doc = "Executes a deposit of a dummy ticket to Deku." in
  Cmd.info "deposit-dummy-ticket" ~version:"%\226\128\140%VERSION%%" ~doc ~exits
    ~man

(* TODO: https://github.com/ocaml/ocaml/issues/11090 *)
let () = Domain.set_name "deku-sandbox"

let default_info =
  let doc =
    "creates, deploys, and starts Deku clusters in a sandbox mode suitable for \
     local development and testnets. BE ADVISED: some of the configuration \
     options used by deku-sandbox are unsafe for production environments. \
     Refer to the production deployment guide." in
  let sdocs = Manpage.s_common_options in
  let exits = Cmd.Exit.defaults in
  Cmd.info "deku-sandbox" ~version:"%\226\128\140%VERSION%%" ~doc ~sdocs ~exits

let _ =
  Cli.make ~info:default_info ()
  |> Cli.add (module Start)
  |> Cli.add (module Setup)
  |> Cli.add (module Teardown)
  |> Cli.add (module Deposit_withdraw_test)
  |> Cli.add (module Deploy_dummy_ticket)
  |> Cli.add (module Deposit_dummy_ticket)
  |> Cli.add (module Load_test)
  |> Cli.add (module Check_liveness)
  |> Cli.eval
