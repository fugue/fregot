package fregot.tests.dyn_01

linker = "dyn_01_a"
value = data.fregot["tests"][linker]["value"]

test_value {
  value == "a"
}
