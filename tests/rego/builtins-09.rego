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
