package invalid_array_01

array_one = ["Hello", "world"]
array_two = ["Fire", "the", "missiles"]

test_array_concat {
  array_big = array.concat(array_one, array_two)
  array_big[_] > 0
}

test_array_slice {
  array_small = array.slice(array_two, 1, 2)
  array_small[_] > 0
}

