module Prefix = struct
  type prefix = string
  type t = prefix

  include Base58.Prefix
end

module With_b58 (P : sig
  type t

  val prefix : string
  val to_raw : t -> string
  val of_raw : string -> t option
end) =
struct
  open P

  let of_b58 string = Base58.simple_decode ~prefix ~of_raw string
  let to_b58 t = Base58.simple_encode ~prefix ~to_raw t
end

(* TODO: exceptions???*)
exception Not_a_string
exception Not_a_b58

(* TODO: this is clearly not in the right file *)
module With_yojson_of_b58 (P : sig
  type t

  val of_b58 : string -> t option
  val to_b58 : t -> string
end) =
struct
  open P

  let t_of_yojson json =
    match json with
    | `String string -> (
        match of_b58 string with Some t -> t | None -> raise Not_a_b58)
    | _ -> raise Not_a_string

  let yojson_of_t t = `String (to_b58 t)
end

(* TODO: this is dumb *)
let rec decode_variant l string =
  match l with
  | of_b58 :: l -> (
      match of_b58 string with
      | Some v -> Some v
      | None -> decode_variant l string)
  | [] -> None

module Base58 = Base58

let unexpected_data ~name =
  Format.kasprintf invalid_arg "Unexpected data (%s)" name

let make_encoding ~name ~to_string ~of_string ~raw_encoding =
  let open Data_encoding in
  let of_string_exn string =
    match of_string string with Some t -> t | None -> unexpected_data ~name
  in
  let json_encoding = conv to_string (Json.wrap_error of_string_exn) string in
  splitted ~binary:(obj1 (req name raw_encoding)) ~json:(def name json_encoding)

module With_encoding (H : sig
  type t

  val name : string
  val prefix : string
  val size : int
  val to_raw : t -> string
  val of_raw : string -> t option
end) =
struct
  open H

  let size = size
  let to_raw = H.to_raw
  let to_string t = Base58.simple_encode ~prefix ~to_raw t
  let of_string string = Base58.simple_decode ~prefix ~of_raw string

  let of_raw_exn string =
    match of_raw string with Some t -> t | None -> unexpected_data ~name

  let encoding =
    make_encoding ~name ~to_string ~of_string
      ~raw_encoding:
        (let open Data_encoding in
        conv to_raw of_raw_exn (Fixed.string size))
end

module With_b58_and_encoding_and_yojson (P : sig
  type t

  val name : string
  val prefix : string
  val size : int
  val to_raw : t -> string
  val of_raw : string -> t option
end) =
struct
  include With_b58 (P)

  include With_yojson_of_b58 (struct
    type t = P.t

    let of_b58 = of_b58
    let to_b58 = to_b58
  end)

  include With_encoding (P)
end
