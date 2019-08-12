# 'any' and 'all'
package fregot.tests.builtins_05

test_any_array_true {
    any([false, true, 100, "foo"])
}

test_any_array_false {
    any([false, 100, "foo"]) == false
}

test_any_array_empty {
    any([]) == false
}

test_any_set_true {
    any({false, true, 100, "foo"})
}

test_any_set_false {
    any({false, 100, "foo"}) == false
}

test_any_set_empty {
    any(set()) == false
}

test_all_array_true {
    all([true, true, true])
}

test_all_array_false {
    all([100, true, "foo"]) == false
}

test_all_array_empty {
    all([])
}

test_all_set_true {
    all({true}) # there is only one such set
}

test_all_set_false {
    all({false, true, "foo"}) == false
}

test_all_set_empty {
    all(set())
}
