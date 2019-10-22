package fregot.tests.repl.debug_cache

allow {
    input.user == "alice"
} {
    input.user == "bob"
}

test_allow {
    allow with input as {"user": "bob"}
}
