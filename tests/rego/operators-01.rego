package fregot.tests.operators_01

test_set_union {
  {1, 2, 3} | {2, 3, 4} == {1, 2, 3, 4}
}

test_assign_equal {
  good := (1 == 1)  # EqualO internally
  good == true      # AssignS internally

  bad := ("hello" == "world")  # EqualO internally
  bad == false                 # AssignS internally
}
