package fregot.tests.else_01

else_01 = 1 {
  false
} else = 2 {
  true
}

test_else_01 {else_01 == 2}

else_02 {
  false
} else {
  false
} else {
  true
}

test_else_02 {else_02}
