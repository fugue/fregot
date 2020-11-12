# 'is_' functions
package fregot.tests.builtins_06

test_is_boolean {
  is_boolean(true)
  is_boolean(false)
  not is_boolean(1)
  not is_boolean([true])
}

test_is_set {
  is_set(set())
  is_set({set()})
  not is_set([set()])
}

test_is_null {
  is_null(null)
  not is_null(false)
}
