# Check for the return values in sets.  This is `true` according to the OPA
# documentation, but the implementation uses the index.
package fregot.tests.sets_01

# 1. A scalar set

set_01 = {"Hello", "World"}

test_set_01 {
    set_01[key] = val
    key == "Hello"
    val == "Hello"
}

# 2. A set generated by a rule

set_02[k] {
    set_01[k]
}

test_set_02 {
    set_02[key] = val
    key == "Hello"
    val == "Hello"
}

# 3. A set generated by a set comprehension

set_03 = {k | set_01[k]}

test_set_03 {
    set_03[key] = val
    key == "Hello"
    val == "Hello"
}

# 4. Straight indexing

test_set_04 {
    set_01["Hello"] == "Hello"
}
