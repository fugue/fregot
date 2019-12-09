package rewind

crashing_rule {
  x = 1
  x + input.msg with input as {"msg": "crash"}
}

failing_rule {
  x = 1
  x = 2
}
