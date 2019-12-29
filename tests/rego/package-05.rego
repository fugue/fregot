# Check that we can "reify" packages into objects.
package fregot.tests.package_05

test_package_as_object {
  # Use `array.concat` to basically erase the type of the module.
  pkgs = array.concat([data.fregot.tests.package_05.reify], [])
  pkg = pkgs[0]

  pkg == {
    "number": 1
  }
}
