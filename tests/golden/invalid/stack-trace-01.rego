package fregot.tests.invalid.stack_trace_01

rule1[x] {
  x = 1
  x + "crash"
}

rule2[x] {
  rule1[x]
}

rule3[x] {
  rule2[x]
}

test_stack_trace {
  rule3[i]
  i == 1
}
