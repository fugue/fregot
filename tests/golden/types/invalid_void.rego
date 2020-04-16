package void

test_assign_void {
  x = array.concat([1], [2], out)
  out == [1, 2]
}

test_array_void {
  all([array.concat([1], [2], out)])
}
