# Strings
package fregot.tests.builtins_01

test_count_01 {
  count(["Hello", "array"]) == 2
  count({"Hello", "set"}) == 2
  count({"Hello": "object"}) == 1
  count("Hello, string") == 13
}

test_lower {
  lower("FOO", "foo")
}

test_split_01 {
  split("provider.aws.us-west-1", ".", splitted)
  splitted[2] == "us-west-1"
}

test_sprintf {
  sprintf("I would like %d slices of %s pizza", [4, "pepperoni"]) ==
      "I would like 4 slices of pepperoni pizza"
}

test_substring {
  substring("foobar", 3, 1 - 2) == "bar"
  substring("foobar", 3, 2) == "ba"
}

test_upper {
  upper("foo", "FOO")
}
