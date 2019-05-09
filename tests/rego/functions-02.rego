package fregot.tests.functions_02

# Turns anything into an array.
as_array(x) = res {
  not is_array(x)
  res = [x]
} else = res {
  res = x
}

test_as_array {
  as_array(1) == [1]
  as_array(["hello"]) == ["hello"]
  as_array({"hello": "world"}) == [{"hello": "world"}]
}
