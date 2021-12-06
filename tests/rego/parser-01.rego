package parser_01

set_1 := {"a"}
set_2 := {"b"}
set_3 := set_1 | set_2

test_set_3 {
  set_3 == {"a", "b"}
}
