package fregot.tests.functions_01

# We can define a function with or without a value.
double_1(x) = y {y = x + x}
double_2(x, y) {y = x + x}

# Two ways of calling this.
test_double_1 {
  2 == double_1(1)
  double_1(1, 2)
}

# Only one way to call this.
test_double_2 {
  double_2(1, 2)
}
