package invalid_array_01

array_bools = ["Hello", "world"]
array_strings = ["Fire", "the", "missiles"]

test_array_concat {
  array_big = array.concat(array_bools, array_strings)
  array_big[_] > 0
}

test_array_slice {
  array_small = array.slice(array_strings, 1, 2)
  array_small[_] > 0
}

test_set_intersection {
  set_bool_string = {true, "Hello"}
  set_string_bool = {"Hello", false}
  set_string = intersection({set_bool_string, set_string_bool})
  set_string[_] > 0
}
