package fregot.tests.json_01

test_json_01 {
  out := json.unmarshal("{\"foo\": 1}")
  out == {"foo": 1}
}

test_json_02 {
  out := json.marshal({"foo": 1, "bar": ["Hello", "World"]})
  out == `{"foo":1,"bar":["Hello","World"]}`
}

test_json_03 {
  json.is_valid("{}")
  not json.is_valid("}{")
}
