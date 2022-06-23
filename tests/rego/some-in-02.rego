package some_in_02

import future.keywords.in

arr = ["foo"]

i = 1

test_i {
	some i, x in arr
	i == 0
	x == "foo"
}

lists = {
	["a", "b", "c"],
	["b", "c"],
}

x = "z"

test_x {
	some [x, "b", y] in lists
	x == "a"
}
