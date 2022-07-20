module Libsecp256k1 = Libsecp256k1.External
open Libsecp256k1

let context =
  let c = Context.create () in
  let rand_value =
    Random.generate 32 |> Cstruct.to_bytes |> Bigstring.of_bytes
  in
  (* TODO: what is this? *)
  match Context.randomize c rand_value with
  | true -> c
  | false -> failwith "Secp256k1 context randomization failed. Aborting."

module Secret = struct
  type secret = Libsecp256k1.Key.secret Libsecp256k1.Key.t
  type t = secret

  let equal = Libsecp256k1.Key.equal

  let compare a b =
    Bigstring.compare (Libsecp256k1.Key.buffer a) (Libsecp256k1.Key.buffer b)

  let to_raw secret =
    Bigstring.to_string (Libsecp256k1.Key.to_bytes context secret)

  include Base58.Make (struct
    type t = secret

    let prefix = Base58.Prefix.secp256k1_secret_key
    let to_raw = to_raw

    let of_raw string =
      Libsecp256k1.Key.read_sk context (Bigstring.of_string string)
      |> Result.to_option
  end)

  let generate () =
    let seed = Random.generate 32 |> Cstruct.to_bytes in
    Libsecp256k1.Key.read_sk_exn context (Bigstring.of_bytes seed)
end

module Key = struct
  type key = Libsecp256k1.Key.public Libsecp256k1.Key.t
  type t = key

  let equal = Libsecp256k1.Key.equal

  let compare a b =
    Bigstring.compare (Libsecp256k1.Key.buffer a) (Libsecp256k1.Key.buffer b)

  let of_secret secret = Libsecp256k1.Key.neuterize_exn context secret
  let to_raw key = Bigstring.to_string (Libsecp256k1.Key.to_bytes context key)

  include Base58.Make (struct
    type t = key

    let prefix = Base58.Prefix.secp256k1_public_key
    let to_raw = to_raw

    let of_raw string =
      Libsecp256k1.Key.read_pk context (Bigstring.of_string string)
      |> Result.to_option
  end)
end

module Key_hash = struct
  open BLAKE2b.BLAKE2b_160

  type key_hash = BLAKE2b.BLAKE2b_160.t
  type t = key_hash

  let equal = equal
  let compare = compare
  let of_key key = hash (Key.to_raw key)

  include With_b58 (struct
    let prefix = Base58.Prefix.secp256k1_public_key_hash
  end)
end

module Signature = struct
  type signature = Sign.plain Sign.t
  type t = signature

  let equal = Sign.equal
  let compare a b = Bigstring.compare (Sign.buffer a) (Sign.buffer b)

  let of_raw string =
    Sign.read context (Bigstring.of_string string) |> Result.to_option

  let size = Sign.plain_bytes
  let zero = of_raw (String.make size '\x00') |> Option.get

  include Base58.Make (struct
    type t = signature

    let prefix = Base58.Prefix.secp256k1_signature

    let to_raw signature =
      Bigstring.to_string (Sign.to_bytes ~der:false context signature)

    let of_raw = of_raw
  end)

  include BLAKE2b.With_alg (struct
    type secret = Secret.t
    type key = Key.t
    type signature = t

    let sign secret hash =
      let hash = Bigstring.of_string hash in
      Sign.sign_exn context ~sk:secret hash

    let verify public signature hash =
      let hash = Bigstring.of_string hash in
      Sign.verify_exn context ~pk:public ~msg:hash ~signature
  end)
end
