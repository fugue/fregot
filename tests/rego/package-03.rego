# This is like `package_02`; it tests some things from `package_01`.  However,
# we use fully qualified names here.
package fregot.tests.package_03

import data.fregot.tests.package_01

test_one {
  data.fregot.tests.package_01.one == 1
}

test_odds {
  data.fregot.tests.package_01.odds[3]
}

test_double {
  data.fregot.tests.package_01.double(3) == 6
}
