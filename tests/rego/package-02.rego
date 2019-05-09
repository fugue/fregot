# This package tests some things from `package_01`.
package fregot.tests.package_02

import data.fregot.tests.package_01

test_one {
  package_01.one == 1
}

test_odds {
  package_01.odds[3]
}

test_double {
  package_01.double(3) == 6
}
