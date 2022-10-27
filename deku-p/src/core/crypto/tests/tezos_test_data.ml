module type Tezos_data = sig
  val public_keys : string list
  val compared_secret_keys : string list
end

module Ed25519_data : Tezos_data = struct
  let public_keys =
    [
      "edpktgopG88M5eE8M6N1ZtHbYzDCXRnGXk9vhNrLnYp9CA6aVyMRXa";
      "edpkvLs7dfWXcdx62iEW5wpYfn8yKPuj446BCRhEbevMMbSSE9G1Yn";
      "edpkvGJ8FdbDSrACkSEzWD1veGeoBQgCTKyX4SVvBc1TBRwcWrbRDQ";
      "edpkvK2woY7vgguhuTZQDVM1hCjbVrEB2dhGVejvgQxHEoGgYqJNuD";
      "edpkutzyeRZkzmcGxQZr7gXTH7Cf7ygDsrn5LSZ2bffHpCeTACB4su";
    ]

  let compared_secret_keys =
    [
      "edsk2kvYWbhbdg6CsgwkZ3svMR76zSJyWUGmpWrRgDRJGJDxZ7aiK3";
      "edsk41Sr6vNDRPenQMNBs11huD26wYMeuJqjFsnC7mNaidVEWuJyh8";
      "edsk44dngys12G6hnRf1VJVTVRcxJWF3nxpPLgKtVJpLzS56eRnYrJ";
      "edsk48oQs2NkiDDNmGfiNnt3NQzL34Cy3tvy9YRCB3RBFXVkkoWnha";
      "edsk4MXvxxHjZKJuW6Rgr1C6tkt6Mwx39o9Tpowco7JjncmSfNb2GF";
    ]
end
