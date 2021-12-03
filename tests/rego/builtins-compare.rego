package builtins_compare

test_lt {
	1 < 2
	"hello" < "world"
	array.concat([1, 2], []) < [1, 2, 1]
	array.concat([1, 2, 1], []) < [1, 3]
}
