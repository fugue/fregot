# '-' operator type mismatches {set, number} <-> {number, set}
package fregot.tests.invalid.builtins_02

test_number_minus_set {
  0 - set()
}

test_set_minus_number {
  set() - 0
}
