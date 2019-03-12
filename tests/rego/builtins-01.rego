package fregot.tests.builtins_01

test_count_01 {
  count(["Hello", "array"]) == 2
  count({"Hello", "set"}) == 2
  count({"Hello": "object"}) == 1
  count("Hello, string") == 13
}

test_split_01 {
  split("provider.aws.us-west-1", ".", splitted)
  splitted[2] == "us-west-1"
}
