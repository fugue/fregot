# https://github.com/open-policy-agent/opa/issues/2497
package bug

english = {
  "one": 1,
  "two": 2,
  "three": 3
}

should_be_1 = ret {
  textual = "one"
  numeric = 1
  ret = {total |
    english[k] = v
    k = textual
    total = sum([numeric | english[_] = numeric])
  }
}

test_should_be_1 {
  should_be_1 == {1}
}
