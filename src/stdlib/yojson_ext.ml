include Yojson

module Safe = struct
  include Safe

  let t_of_yojson t = t
  let yojson_of_t t = t
end
