package fregot.tests.invalid.obj_01

inconsistent[k] = 1 {k = "hi"}
inconsistent[k] = 2 {k = "hi"}

test_inconsistency {
  inconsistent == {"hi": 1}
}
