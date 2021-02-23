# Regex tests.
package fregot.tests.builtins_09

test_regex_split {
  regex.split(":", ":foo::bar") == ["", "foo", "", "bar"]
  regex.split(":+", ":foo::bar") == ["", "foo", "bar"]
}

test_regex_is_valid {
  regex.is_valid(":+")
  not regex.is_valid("+")
}

test_regex_find_n {
  regex.find_n("[oa]+", "foo bar", -1) == ["oo", "a"]
  regex.find_n("[oa]+", "foo bar", 0) == []
  regex.find_n("[oa]+", "foo bar", 1) == ["oo"]
  regex.find_n("[oa]+", "foo bar", 10) == ["oo", "a"]
}
