package fregot.tests.types_02

yield_2(path) = ret {
  ret := [[], path]
} else = ret {
  ret := [path, path]
}

test_yield {
  [one, two] = yield_2("foo")
  # This should not trigger a type error
  foo = [two, one]
}
