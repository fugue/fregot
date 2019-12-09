package debug_error

test_crash {
  x = 1
  x + input.msg with input as {"msg": "crash"}
}
