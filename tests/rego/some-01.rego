package fregot.tests.some_01

numbers = ["one", "two", "three", "four"]

i = 2

set_i[w] {
    i % 2 == 0
    w = numbers[i]
}

j = 2

set_j[w] {
    some j
    j % 2 == 0
    w = numbers[j]
}

test_set_i {
    set_i == {"three"}
}

test_set_j {
    set_j == {"one", "three"}
}
