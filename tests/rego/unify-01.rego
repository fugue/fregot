package fregot.tests.unify_01

test_unify_01 {
  to_number(num) == 1234
  split("routes.1234.cidr_block", ".", [_, num, _])
}
