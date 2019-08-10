package fregot.tests.invalid.unknown_var_02

test_kv[k] = v {
  # k is not assigned here.
  v = 1
} {
  # v is not assigned here.
  k = 2
} {
  # ok.
  v = 1
  k = 1
}
