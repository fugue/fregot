# A function call without a body.
package fregot.tests.bodies_02

bigger_than_2(xs) = {x |
  x = xs[_]
  x > 2
}

test_bigger_than_2 {
  bigger_than_2([1, 2, 3, 4, 5]) == {3, 4, 5}
}
