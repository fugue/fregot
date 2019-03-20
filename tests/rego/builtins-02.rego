# Aggregates
package fregot.tests.builtins_02

test_sum {
  sum([]) == 0
  sum([1, 1]) == 2
  sum([1.2, 1]) == 2.2
  sum([0.5, 0.5, 0.5]) == 1.5
}

test_product {
  product([]) == 1
  product([1, 2, 3, 4, 5]) == 120
  product([0.5, 0.5]) == 0.25
}
