package fregot.tests.refs_01

access_array_with_key = 1 {
  input.array.key
} else = 2 {
  true
}

test_access_array_with_key {
  access_array_with_key with input as {"array": [1, 2, 3]}
}

input_has_tags {
  input.tags[_] = _
}

test_access_null {
  not input_has_tags with input as {"tags": null}
}
