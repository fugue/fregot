package fregot.tests.nested_comp_01

nested = ret {
  ret = [elem |
    index = 1
    elem = {"key": 1 |
      index == 1
    }
  ]
}

test_nested {
  nested == [{"key": 1}]
}
