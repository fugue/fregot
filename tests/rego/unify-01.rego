package fregot.tests.unify_01

test_unify_01 {
  to_number(num) == 1234
  split("routes.1234.cidr_block", ".", [_, num, _])
}

test_unify_02 {
  [_, num, _] := json.unmarshal("[1, 2, 3]")
  num == 2
}
