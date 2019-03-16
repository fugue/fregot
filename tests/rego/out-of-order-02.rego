package fregot.tests.out_of_order_02

list = [0, 0, 0]

# This example shows that we also need to order rule values.
comp = [i | i > 1; list[i]]

test_comp {
    comp == [2]
}
