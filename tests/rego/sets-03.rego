# This triggers a very specific bug in the way set indexing used to work, where
# unifying the final computed index with the given index was skipped.
package fregot.tests.sets_03

allowed_ports[443] {
  true
}

test_allow {
  not allowed_ports[80]
}
