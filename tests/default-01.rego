# Tests for the default keyword.
package fregot.tests.default_01

default def1 = 1
def1 = 2 {false}
test_def1 {def1 = 1}

default def2 = 1
def2 = 2
test_def2 {def2 = 2}
