# The `trim` family.
package fregot.tests.builtins_08

test_trims {
  trim("__a_a__", "_") == "a_a"
  trim_left("__a_a__", "_") == "a_a__"
  trim_prefix("__a_a__", "_") == "_a_a__"
  trim_prefix("__a_a__", "___") == "__a_a__"
  trim_right("__a_a__", "_") == "__a_a"
  trim_suffix("__a_a__", "_") == "__a_a_"
  trim_suffix("__a_a__", "a") == "__a_a__"
  trim_space("\t a_a\n") == "a_a"
}
