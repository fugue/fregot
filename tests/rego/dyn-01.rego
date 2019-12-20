package fregot.tests.dyn_01

linker = "dyn_01_a"
linked_value = data.fregot["tests"][linker]["value"]

test_value {
  linked_value == "a"
}
