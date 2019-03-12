# Some tests for with modifiers.
package fregot.tests.with_01

allow {
    input.ok
}

test_allow_01 {
    allow with input.ok as true
}

test_allow_02 {
    allow with input as {"ok": true}
}
