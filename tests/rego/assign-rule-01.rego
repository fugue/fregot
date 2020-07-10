package fregot.tests.assign_rule_01

pi := 3.14

allow := true {
  input.age >= 18
}

test_pi {
  pi == 3.14
}

test_allow {
  allow with input as {"age": 20}
  not allow with input as {"age": 17}
}
