package fregot.tests.json

import data.json_policy
import data.common_policy
import data.numbers

test_json_policy {
  json_policy.filename == "stdlib.h"
}

test_common_policy {
  common_policy.numbers == {"one": 1, "two": 2}
}

test_numbers {
  sum(numbers) == 6
}
