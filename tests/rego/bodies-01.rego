# Don't worry, this is not where the bodies are buried.  In this file, we have
# some tests for rules that have multiple bodies.
package fregot.tests.bodies_01

bodies_01 = 1 {
  false
} {
  true
}

test_bodies_01 {bodies_01 == 1}

# NOTE(jaspervdj): I wanted to add more tests here but apparently OPA doesn't
# really support mixing multiple bodies together with `else`.  Oh, well.

# bodies_02 = 1 {
#   false
# } else = 2 {
#   false
# } {
#   true
# }

bodies_03[k] {
  k = 1
} {
  k = 2
}

test_bodies_3 {bodies_03 == {1, 2}}

# bodies_04 = 1 {
#   false
# } {
#   true
# } else = 2 {
#   true
# }

bodies_04(x, y) = z {
  false
  z = 3
} {
  z = x + y
}

test_bodies_04 {bodies_04(1, 1) == 2}
