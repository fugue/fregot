package in_01

import future.keywords.in

listy = ["foo"]

test_some_in_listy {
    "foo" in listy
    0, "foo" in listy
    not "bar" in listy
    not 1, "foo" in listy
    not 0, "bar" in listy
}

setty = {"foo"}

test_some_in_setty {
    "foo" in setty
    not "bar" in setty
}

mappy = {"foo": "bar"}

test_some_in_mappy {
    "bar" in mappy
    "foo", "bar" in mappy
    not "qux" in mappy
    not "qux", "bar" in mappy
    not "foo", "qux" in mappy
}
