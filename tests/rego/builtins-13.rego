# Number and bits related tests
package fregot.tests.builtins_13

test_numbers_range {
	numbers.range(1, 1) == [1]
	numbers.range(1, 5) == [1, 2, 3, 4, 5]
	numbers.range(5, 1) == [5, 4, 3, 2, 1]
	numbers.range(5, 4) == [5, 4]
}

test_bits_or {
	bits.or(1, 1) == 1
	bits.or(2, 5) == 7
	bits.or(10, 101) == 111
}

test_bits_and {
	bits.and(1, 1) == 1
	bits.and(2, 10) == 2
	bits.and(10, 111) == 10
	bits.and(1, 2) == 0
}
