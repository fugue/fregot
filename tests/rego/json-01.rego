package fregot.tests.json_01

test_json_01 {
  out := json.unmarshal("{\"foo\": 1}")
  out == {"foo": 1}
}
