# Regex tests.
package fregot.tests.builtins_09

test_regex_split {
  regex.split(":", ":foo::bar") == ["", "foo", "", "bar"]
  regex.split(":+", ":foo::bar") == ["", "foo", "bar"]
}
