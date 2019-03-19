# Aggregates
package fregot.tests.builtins_02

test_sum {
  sum([]) == 0
  sum([1, 1]) == 2
  sum([1.2, 1]) == 2.2
  sum([0.5, 0.5, 0.5]) == 1.5
}
