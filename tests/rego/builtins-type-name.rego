package fregot.tests.builtins_type_name

test_type_name {
	type_name("s") == "string"
	type_name(0) == "number"
	type_name(true) == "boolean"
	type_name(set()) == "set"
	type_name({1, 2}) == "set"
	type_name([]) == "array"
	type_name({}) == "object"
	type_name(null) == "null"
}
