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

test_regex_find_all_string_submatch_n {
  regex.find_all_string_submatch_n(
    "([a-z]+)@([a-z]+)",
    "test@example or foo@bar and maybe qux@wop",
    -1
  ) == [
    ["test@example", "test", "example"],
    ["foo@bar", "foo", "bar"],
    ["qux@wop", "qux", "wop"]
  ]
  regex.find_all_string_submatch_n(
    "([a-z]+)@([a-z]+)",
    "test@example or foo@bar and maybe qux@wop",
    2
  ) == [
    ["test@example", "test", "example"],
    ["foo@bar", "foo", "bar"],
  ]
}
