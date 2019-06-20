package fregot.tests.ref_line_parse

test_ref_line_parse {
  # This should not be parsed as _[_]
  "123" = _
  [_] = [2]
}
