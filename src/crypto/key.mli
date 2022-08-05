(* TODO: make those GADTs *)
type key =
  | Ed25519 of Ed25519.Key.t
  | Secp256k1 of Secp256k1.Key.t
  | P256 of P256.Key.t

type t = key [@@deriving eq, ord, yojson]

(* repr *)
val encoding : key Data_encoding.t
val of_b58 : string -> key option
val to_b58 : key -> string

(* operations *)
val of_secret : Secret.t -> t
