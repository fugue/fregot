# This package only defines some rules and functions; they are used in the
# `package_02` file.
package fregot.tests.package_01

one = 1

numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

odds[x] {
  numbers[_] = x
  x % 2 != 0
}

double(x) = y {
  y = x * 2
}
