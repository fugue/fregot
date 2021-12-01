# Number and bits related tests
package fregot.tests.builtins_13

test_abs {
	abs(1) == 1
	abs(-2) == 2
	abs(0) == 0
}

test_ceil {
    ceil(1) == 1
    ceil(1.2) == 2
}

test_floor {
    floor(1) == 1
    floor(1.2) == 1
}

test_round {
    round(1) == 1
    round(1.2) == 1
    round(1.6) == 2
}

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

test_bits_xor {
	bits.xor(1, 1) == 0
	bits.xor(8, 24) == 16
	bits.xor(10, 102) == 108
}

test_bits_lsh {
	bits.lsh(1, 0) == 1
	bits.lsh(1, 1) == 2
	bits.lsh(8, 24) == 134217728
}

test_bits_rsh {
	bits.rsh(1, 0) == 1
	bits.rsh(1, 1) == 0
	bits.rsh(24, 2) == 6
}

test_bits_negate {
	bits.negate(1) == -2
	bits.negate(2) == -3
	bits.negate(-10) == 9
}
