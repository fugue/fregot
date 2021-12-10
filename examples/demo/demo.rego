package fregot.examples.demo

# Whitelisted AMIs
approved_amis = {
  "ami-04b9e92b5572fa0d1", "ami-0b69ea66ff7391e80"
}

# All AMIs in the input
amis[ami] {
    ami = input.resource_changes[_].change.after
    startswith(ami, "ami-")
}

# If the AMI does not appear in the list of whitelisted AMIs,
# it's an invalid AMI, so return true
invalid_ami(ami) {
   not approved_amis[ami]
}

# Return true if there are any invalid AMIs
deny {
    ami = amis[ami]
    invalid_ami(ami)
}

# This test should allow the specified AMIs
test_allow {
    not deny with input as {"resource_changes": [
  {"change": {"after": {"ami": "ami-04b9e92b5572fa0d1"}}},
  {"change": {"after": {"ami": "ami-0b69ea66ff7391e80"}}}
]}
}

# This test should deny the specified AMIs
test_deny {
    deny with input as {"resource_changes": [
  {"change": {"after": {"ami": "ami-0"}}},
  {"change": {"after": {"ami": "ami-0b69ea66ff7391e80"}}}
]}
}