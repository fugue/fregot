package fregot.tests.out_of_order_01

test_01 {
    z < 2
    z = 1
}

test_02 {
    z < 2
    r = [a | a > 2; a = 1]
    count(r, z)
}
