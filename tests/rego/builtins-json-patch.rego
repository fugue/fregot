package fregot.tests.builtins_json_patch

import data.json_spec_tests.json_spec_tests
import data.json_tests.json_tests

all_cases[k] = t {
  t := data.json_patch_cases[p]["cases"][i]
  k := sprintf("%s/%d", [p, i])
}

passed_cases[k] = t {
  t := all_cases[k]
  t.expected == json.patch(t.doc, [p | p = t.patch[_]])
} {
  # Some cases ("test" ones in particular) don't have an expected value but
  # are still tests that we want to run.
  t := all_cases[k]
  not t.expected
  not t.error
  _ = json.patch(t.doc, [p | p = t.patch[_]])
} {
  t := all_cases[k]
  _ = t.error
  not json.patch(t.doc, [p | p = t.patch[_]])
} {
  # Some cases are specifically disabled for the OPA implementation.
  t := all_cases[k]
  t.opa_disabled == true
}

failed_cases[k] = t {
  t := all_cases[k]
  not passed_cases[k]
}

test_no_failed_cases {
  failed_cases == {}
}
