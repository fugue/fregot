package invalid_array_01

array_one = ["Hello", "world"]
array_two = ["Fire", "the", "missiles"]

test_array {
  array_big = array.concat(array_one, array_two)
  array_big[_] > 0
}
