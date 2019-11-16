# 'is_' functions
package fregot.tests.builtins_06

test_is_boolean {
  is_boolean(true)
  is_boolean(false)
  not is_boolean(1)
  not is_boolean([true])
}
