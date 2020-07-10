package fregot.tests.invalid.assign_rule_01

# Cannot use `:=` with `default`
default allow := false

# Cannot use multiple `:=`
pi := 3.14
pi = 3.14

# Functions should not use `:=`
double(x) := ret {ret := x + x}
