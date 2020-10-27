package fregot.tests.indirect_references

p[x] {
  [1, 2, 3][x]
}

test_p {
  p == {0, 1, 2}
}

test_split_01 {
  split("foo.bar", ".")[0] == "foo"
}

q[x] {
  split(split("foo.bar:qux", ".")[_], ":")[i] = x
}

test_q {
  q == {"foo", "bar", "qux"}
}
