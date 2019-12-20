package fregot.tests.sets_02

deny[msg] {
    msg = "go away"
    input.name == "intruder"
}

test_deny {
    deny == {"go away"} with input as {"name": "intruder"}
    deny == set() with input as {"name": "legitimate"}
}
