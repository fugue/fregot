# Number related tests
package fregot.tests.builtins_13

test_numbers_range {
	numbers.range(1, 1) == [1]
	numbers.range(1, 5) == [1, 2, 3, 4, 5]
	numbers.range(5, 1) == [5, 4, 3, 2, 1]
	numbers.range(5, 4) == [5, 4]
}
