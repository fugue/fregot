# Object related tests.
package fregot.tests.builtins_12

test_object_remove {
	x := {"foo": 100, "bar": true, "baz": {"q": "a"}}
	object.remove(x, ["foo", "baz"]) == {"bar": true}
	object.remove(x, {"foo", "baz"}) == {"bar": true}
	object.remove(x, {"foo": "what", "baz": "ever"}) == {"bar": true}
	object.remove(x, {"foo", "bar"}) == {"baz": {"q": "a"}}
}

test_object_filter {
	x := {"foo": 100, "bar": true, "baz": {"q": "a"}}
	object.filter(x, ["foo", "baz"]) == {"foo": 100, "baz": {"q": "a"}}
	object.filter(x, {"foo", "baz"}) == {"foo": 100, "baz": {"q": "a"}}
	object.filter(x, {"foo": "what", "baz": "ever"}) == {"foo": 100, "baz": {"q": "a"}}
	object.filter(x, {"foo", "bar"}) == {"foo": 100, "bar": true}
}
