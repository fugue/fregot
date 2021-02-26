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

test_object_get {
	x := {"foo": 100, "bar": true, "baz": {"q": "a"}}
	object.get(x, "foo", "def") == 100
	object.get(x, "faz", "def") == "def"
	object.get(x, "baz", 100) == {"q": "a"}
	object.get(x, "faz", {"a": "q"}) == {"a": "q"}
}

test_object_union {
    object.union({}, {}) == {}
    object.union(
        {"a": 1, "b": 2, "c": {"d": 3}},
        {"a": 7, "c": {"d": 4, "e": 5}}
    ) == {"a": 7, "b": 2, "c": {"d": 4, "e": 5}}
    object.union(
        {"a": 7, "c": {"d": 4, "e": 5}},
        {"a": 1, "b": 2, "c": {"d": 3}}
    ) == {"a": 1, "b": 2, "c": {"d": 3, "e": 5}}
}
