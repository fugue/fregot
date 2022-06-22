package some_in_02

listy = ["foo"]

test_some_in_listy {
    some a in listy
    a == "foo"

    some i, b in listy
    b == "foo"
    i == 0
}

setty = {"foo"}

test_some_in_setty {
    some a in setty
    a == "foo"

    some i, b in setty
    b == "foo"
    i == "foo"
}

mappy = {"foo": "bar"}

test_some_in_mappy {
    some a in mappy
    a == "bar"

    some i, b in mappy
    b == "bar"
    i == "foo"
}
