# JWT tests.
package fregot.tests.builtins_10

secret_password = "hunter2 hunter2 hunter2 hunter2 hunter2"

example_01 = {
  "header": {"typ": "JWT", "alg": "HS256"},
  "payload": {"iss": "alice"},
  "key": {
    "kty": "oct",
    "k": base64.encode(secret_password)
  }
}

test_round_trip_01 {
  jwt = io.jwt.encode_sign(example_01.header, example_01.payload, example_01.key)
  io.jwt.decode(jwt, [example_01.header, example_01.payload, _])
  io.jwt.decode_verify(jwt, {"secret": secret_password},
      [true, example_01.header, example_01.payload])
}

test_round_trip_02 {
  jwt = io.jwt.encode_sign(example_01.header, example_01.payload, example_01.key)
  io.jwt.decode_verify(jwt, {"secret": "abc123"}, [false, _, _])
}

test_decode_sig_01 {
  [header, payload, sig] = io.jwt.decode("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJhbGljZSJ9.OsFj3QHjXgAhTwL7FQ2xjvJwL8FTwnQ1d3x42SuImH8")
  header == {"alg": "HS256", "typ": "JWT"}
  payload == {"iss": "alice"}
  sig == "3ac163dd01e35e00214f02fb150db18ef2702fc153c27435777c78d92b88987f"
}
