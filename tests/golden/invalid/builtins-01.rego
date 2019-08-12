# 'all' and 'any'
package fregot.tests.invalid.builtins_01

test_all_object {
  all({"foo": true, "bar": false, "baz": 100})
}

test_any_object {
  any({"foo": true, "bar": false, "baz": 100})
}
