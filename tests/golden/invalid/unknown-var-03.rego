package fregot.tests.invalid.unkown_var_03

double(x) = y {
  y = x + x
}

test_double {
  double(x, y)
  y == 2
}
