package fregot.tests.types_01

get_4th(arr) = ret {
  is_array(arr)
  ret = arr[3]
} else = ret {
  ret = 0
}

test_get_4th_array {
  get_4th([1, 2, 3, 4, 5]) == 4
  get_4th("hi") == 0
}
