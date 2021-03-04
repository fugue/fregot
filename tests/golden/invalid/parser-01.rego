package fregot.tests.invalid.parser_01

# This is a common mistake and the error is bad since `function` is seen as
# a boolean rule with no body.
function double(x) = y {
  y = x * 2
}
