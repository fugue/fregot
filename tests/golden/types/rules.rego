package rules

some_number = 4 {
  true
}

some_set[i] {
  i = 1
} {
  i = 2
} {
  i = "hello"
}

some_object[k] = v {
  k = "one"
  v = 1
} {
  k = "two"
  v = 2
}
