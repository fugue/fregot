# `walk` for graph traversal
package fregot.tests.builtins_07

document = {
  "name": "Doe",
  "address": {
    "street": "Long Street",
    "number": 10
  },
  "email": ["doe@example.com", "admin@doe.com"]
}

test_walk {
  walk(document, [[], document])
  walk(document, [["name"], "Doe"])
  walk(document, [["address"],{"number":10,"street":"Long Street"}])
  walk(document, [["address","street"],"Long Street"])
  walk(document, [["address","number"],10])
  walk(document, [["email"],["doe@example.com","admin@doe.com"]])
  walk(document, [["email",0],"doe@example.com"])
  walk(document, [["email",1],"admin@doe.com"])
}

test_walk_set {
  walk({"a", "b"}, [["a"], "a"])
  walk({"a", "b"}, [["b"], "b"])
}

test_walk_index {
  [_, address] := walk(document)
  _ := address.street
  _ := address.number
}
