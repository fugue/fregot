package fregot.tests.some_02

x := { "foo", "bar" }

allow {
  some a, b
  x[a] == x[b]
}
