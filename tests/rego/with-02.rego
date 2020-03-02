# Regression test; scoping of `with` should be limited.
package fregot.tests.with_02

the_input_size = ret {
  ret = input.size
}

test_input_size {
  size = the_input_size with input as {"size": 5}
  size == 5
  not the_input_size
}
