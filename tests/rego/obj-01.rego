package fregot.tests.obj_01

test_obj_equality {
  {1: "Hello", 2: "World!"} == {2: "World!", 1: "Hello"}
}

test_empty_obj {
  is_object({})
}
