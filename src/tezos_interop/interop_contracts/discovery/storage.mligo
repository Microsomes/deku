#import "errors.mligo" "Errors"
#import "parameter.mligo" "Parameter"

type uri = Parameter.Types.uri
type nonce = Parameter.Types.nonce

module Types = struct
  type t = (key_hash, (nonce * uri)) big_map
end

module Utils = struct
  let check_nonce (storage : Types.t) (key_hash : key_hash) (nonce : nonce) =
    match Big_map.find_opt key_hash storage with
    | Some (old_nonce, _) -> 
      assert_with_error (nonce > old_nonce) Errors.old_nonce
    | None -> ()

  let check_signature (key : key) (uri : uri) (nonce : nonce) (signature : signature) =
      let packed_data = Bytes.pack (nonce, uri) in
      assert_with_error (Crypto.check key signature packed_data) Errors.invalid_signature
end