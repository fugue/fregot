# Time functions in particular
package fregot.tests.builtins_04

test_time_now_ns {
  # This test will be run in the future, not the past.
  time.now_ns() > 1560965817971737362
}

test_time_date {
  time.date(1560965817971737362) == [2019, 6, 19]
}

test_time_parse_rfc3339_ns {
  time.parse_rfc3339_ns("2016-06-08T15:18:42+00:00") == 1465399122000000000
  time.parse_rfc3339_ns("2019-06-19T17:39:02+00:00") == 1560965942000000000
}
