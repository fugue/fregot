package fregot.tests.repl.input_doc

security_groups[id] = resource {
  resource = input.resources[id]
  resource.type == "security_group"
}
