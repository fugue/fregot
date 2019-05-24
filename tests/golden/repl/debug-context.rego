# Check that we can access the right things from the debug context.  In
# particular, the input and rules form the "current" package.
package debug_context

security_groups[id] = security_group {
  security_group = input.resources[id]
  security_group.type = "security_group"
}

valid_security_groups[id] = security_group {
  security_group = security_groups[id]
  security_group.good_boy
}

test_valid_security_groups {
  valids = valid_security_groups with input as {
    "resources": [
      {"id": "1", "type": "security_group", "good_boy": true},
      {"id": "2", "type": "security_group", "good_boy": false},
      {"id": "3", "type": "virtual_machine"}
    ]
  }
}
