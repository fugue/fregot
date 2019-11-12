package fregot.tests.obj_02

inconsistent[k] = 1 {k = "hi"}
inconsistent[k] = 2 {k = "hi"}

test_inconsistency {
  # It's a bit weird because OPA will not allow you to write `inconsistent`
  # (that evaluates into an "object keys must be unique" error) but we are
  # allowed to get multiple rows if we address it as a rule.
  inconsistent["hi"] == 2
}
