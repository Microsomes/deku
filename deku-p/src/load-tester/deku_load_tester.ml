open Deku_concepts
open Deku_crypto
open Deku_protocol
open Deku_stdlib

(* FIXME: this code is duplicated *)
module Signed_operation = struct
  type t = {
    key : Key.t;
    signature : Signature.t;
    initial : Operation.Initial.t;
  }
  [@@deriving yojson]

  let of_signed signed =
    let (Operation.Signed.Signed_operation { key; signature; initial }) =
      signed
    in
    { key; signature; initial }

  let to_signed repr =
    let { key; signature; initial } = repr in
    Operation.Signed.make_with_signature ~key ~signature ~initial
end

let get_body response =
  match response with
  | Error err -> Error (Piaf.Error.to_string err)
  | Ok Piaf.Response.{ status; body; _ } -> (
      match Piaf.Status.is_successful status with
      | false ->
          Error
            (Format.sprintf "receive code response: %s"
               (Piaf.Status.to_string status))
      | true -> (
          match Piaf.Body.to_string body with
          | Ok string -> Ok (Yojson.Safe.from_string string)
          | Error err -> Error (Piaf.Error.to_string err)))

let send_operation ~env ~api_url operation_json =
  Eio.Switch.run @@ fun sw ->
  let body = Piaf.Body.of_string operation_json in
  let response = Piaf.Client.Oneshot.post ~body ~sw env api_url in
  get_body response

type level_response = { level : Level.t } [@@deriving of_yojson]

let level ~sw ~env ~api_url =
  let level_uri = Uri.with_path api_url "/api/v1/chain/level" in
  let response = Piaf.Client.Oneshot.get ~sw env level_uri in
  get_body response
  |> Result.map level_response_of_yojson
  |> Result.map (fun { level } -> level)

type params = {
  domains : int; [@default 8]
  api_url : Uri.t; [@default Uri.of_string "http://localhost:8080"]
  secret : Ed25519.Secret.t; [@default Ed25519.Secret.generate ()]
  n : int; [@default 1]
}
[@@deriving cmdliner]

let main params =
  let { domains; api_url; secret; n } = params in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let open Operation in
  Parallel.Pool.run ~env ~domains @@ fun () ->
  let identity = Identity.make (Secret.Ed25519 secret) in
  let rng = Stdlib.Random.State.make_self_init () in
  let level = level ~sw ~env ~api_url in
  let level =
    match level with
    | Ok level -> level
    | Error str ->
        print_endline str;
        exit 1
  in

  let operations =
    Parallel.init_p n (fun _ ->
        let nonce =
          Stdlib.Random.State.bits64 rng
          |> Int64.abs |> Z.of_int64 |> N.of_z |> Option.get |> Nonce.of_n
        in
        let operation = Signed.noop ~identity ~level ~nonce in
        Signed_operation.of_signed operation
        |> Signed_operation.yojson_of_t |> Yojson.Safe.to_string)
  in

  let api_url = Uri.with_path api_url "/api/v1/operations" in

  print_endline "sending operations";
  let start = Unix.gettimeofday () in
  let result = Parallel.map_p (send_operation ~env ~api_url) operations in
  let stop = Unix.gettimeofday () in

  print_endline @@ Format.sprintf "time: %fs" (stop -. start);

  let _ =
    Parallel.map_p
      (fun result ->
        match result with
        | Error reason -> print_endline reason
        | Ok json ->
            print_endline
              (Yojson.Safe.Util.member "hash" json
              |> Yojson.Safe.Util.to_string_option
              |> Option.value ~default:"bad response"))
      result
  in
  exit 0

let () =
  let info = Cmdliner.Cmd.info "deku-load-tester" in
  let term = Cmdliner.Term.(const main $ params_cmdliner_term ()) in
  let cmd = Cmdliner.Cmd.v info term in
  exit (Cmdliner.Cmd.eval ~catch:true cmd)
