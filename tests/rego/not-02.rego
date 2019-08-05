package fregot.tests.not_02

empty_complete_rule {
  false
}

test_empty_complete_rule {
  not empty_complete_rule
}

empty_set_rule[k] {
  k = 1
  k = 2
}

test_empty_set_rule {
  empty_set_rule
}
