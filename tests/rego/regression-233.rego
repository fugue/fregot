# <https://github.com/fugue/fregot/issues/233>
package test.policy

policies = {"1": {"name": "test"}}

match[policy.name] {
   policy := policies[input.id]
   policy.name == input.name
}

test_match {
  r := match[_] with input as {"id": "1", "name": "test"}
  r == "test"
}
