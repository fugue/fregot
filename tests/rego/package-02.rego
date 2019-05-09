# This package tests some things from `packages_01`.
package fregot.tests.packages_02

import data.fregot.tests.packages_01

test_one {
  packages_01.one == 1
}

test_odds {
  packages_01.odds[3]
}

test_double {
  packages_01.double(3) == 6
}
