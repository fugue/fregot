package fregot.examples.ami_id

default allow = false
 
# Whitelisted AMIs
approved_amis = {
  "ami-04b9e92b5572fa0d1", "ami-0b69ea66ff7391e80"
}

# All AMIs in the input
amis[ami] { 
    ami = input.resource_changes[_].change.after.ami
    startswith(ami, "ami-")
}

# All AMIs in the input that are not whitelisted
unapproved_amis[ami] { 
    amis[ami]
    not approved_amis[ami]
}

# Return true if there are no unapproved AMIs
allow {
    count(unapproved_amis) == 0
}

# This test should allow the specified AMIs
test_allow {
    allow with input as {"resource_changes": [
  {"change": {"after": {"ami": "ami-04b9e92b5572fa0d1"}}},
  {"change": {"after": {"ami": "ami-0b69ea66ff7391e80"}}}
]}
}

# This test should not allow the specified AMIs
test_deny {
    not allow with input as {"resource_changes": [
  {"change": {"after": {"ami": "ami-0"}}},
  {"change": {"after": {"ami": "ami-0b69ea66ff7391e80"}}}
]}
}