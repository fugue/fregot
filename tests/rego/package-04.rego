# Check that we can "iterate" over packages.
# we use fully qualified names here.
package fregot.tests.package_04

packages_as_object[pkg] = val {
  data.fregot.tests.package_04.pkgs[pkg].value = val
}

test_packages_as_object {
  packages_as_object == {"a": 1, "b": 2}
}
