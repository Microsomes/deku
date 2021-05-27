open Helpers;
open Mirage_crypto_ec;

[@deriving yojson]
type t = {
  // TODO: what is the name of a signature?
  signature: string,
  public_key: Address.t,
};
// TODO: is it safe to compare only the signature?
let compare = (a, b) => String.compare(a.signature, b.signature);
let public_key = t => t.public_key;
let sign = (~key, hash) => {
  let signature =
    Cstruct.of_string(SHA256.to_string(hash))
    // TODO: isn't this double hashing? Seems weird
    |> Ed25519.sign(~key)
    |> Cstruct.to_string;
  let public_key = Ed25519.pub_of_priv(key);
  {signature, public_key};
};
let verify = (~signature, hash) =>
  Ed25519.verify(
    ~key=signature.public_key,
    ~msg=Cstruct.of_string(SHA256.to_string(hash)),
    Cstruct.of_string(signature.signature),
  );
module type S = {
  type value;
  type signature = t;
  type t =
    pri {
      value,
      signature,
    };
  let sign: (~key: Ed25519.priv, value) => t;
  let verify: (~signature: signature, value) => bool;
};
module Make = (P: {
                 type t;
                 let hash: t => SHA256.t;
               }) => {
  type value = P.t;
  type signature = t;
  type t = {
    value,
    signature,
  };
  let sign = (~key, value) => {
    // TODO: maybe reuse this hash?
    let signature = P.hash(value) |> sign(~key);
    {value, signature};
  };
  // TODO: pretty sure this should be reused
  let verify = (~signature, value) => P.hash(value) |> verify(~signature);
};
