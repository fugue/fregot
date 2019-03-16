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

to_english(x) = "one" {
  x == 1
} else = "two" {
  x == 2
} else = "some number" {
  true
}

test_to_english {
  to_english(1) == "one"
  to_english(2) == "two"
  to_english(3) == "some number"
}
