# 'any' and 'all'
package fregot.tests.builtins_05

test_any_array_true {
    any([false, true])
}

test_any_array_false {
    any([false, false]) == false
}

test_any_array_empty {
    any([]) == false
}

test_any_set_true {
    any({false, true})
}

test_any_set_false {
    any({false}) == false
}

test_any_set_empty {
    any(set()) == false
}

test_all_array_true {
    all([true, true, true])
}

test_all_array_false {
    all([true, true, false]) == false
}

test_all_array_empty {
    all([])
}

test_all_set_true {
    all({true}) # there is only one such set
}

test_all_set_false {
    all({false}) == false
}

test_all_set_empty {
    all(set())
}
