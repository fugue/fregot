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

es256_token = "eyJ0eXAiOiAiSldUIiwgImFsZyI6ICJFUzI1NiJ9.eyJuYmYiOiAxNDQ0NDc4NDAwLCAiaXNzIjogInh4eCJ9.lArczfN-pIL8oUU-7PU83u-zfXougXBZj6drFeKFsPEoVhy9WAyiZlRshYqjTSXdaw8yw2L-ovt4zTUZb2PWMg"
jwks = `{
    "keys": [{
        "kty":"EC",
        "crv":"P-256",
        "x":"z8J91ghFy5o6f2xZ4g8LsLH7u2wEpT2ntj8loahnlsE",
        "y":"7bdeXLH61KrGWRdh7ilnbcGQACxykaPKfmBccTHIOUo"
    }]
}`

test_using_jkws_01 {
  io.jwt.decode_verify(es256_token, {
      "cert": jwks,
      "iss": "xxx",
    },
    [
      true,
      {"alg": "ES256","typ": "JWT"},
      {"iss": "xxx","nbf": 1444478400},
    ]
  )
}
