package parser_02

test_split {
	attrs := {
		["foo", "10", "bar", "20"],
		["foo", "20", "bar", "30"],
		["foo", "20", "qux", "30"],
	}

	# Parser used to get confused, thinking the RHS was `attrs[_]["foo"]`
	count([attr |
		attr := attrs[_]
		["foo", _, "bar", _] = attr
	]) == 2
}
