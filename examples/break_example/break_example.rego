package fregot.examples.break_example

function_a {
  a = "Welcome to function a!"
  true
}

function_b {
  b = "Now you are in function b!"
  true
}

test_step {
  function_a
  function_b
  c = "If you can read this, you are in test_step!"
}
