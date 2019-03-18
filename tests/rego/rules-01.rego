# Some general tests for rule.
package fregot.tests.rules_01

fields = {"Age": 30, "Name": "Tommy"}

obj_rule[k] = v {fields[k] = v}
set_rule[v]     {fields[k] = v}

test_obj_rule {
  o = obj_rule  # Fully evaluate.
  o["Age"]
}

test_set_rule {
  s = set_rule  # Fully evaluate.
  s["Tommy"]
}
