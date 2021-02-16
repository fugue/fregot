# More complex unification where we pattern match whole objects.
package fregot.tests.unify_01

rewrite(value) = id {
  value = [id, "Arn"]
} else = id {
  value = {"Fn::GetAtt": [id, "Arn"]}
} else = id {
  value = {"Ref": id}
}

test_rewrite {
  "Api" == rewrite(input) with input as ["Api", "Arn"]
  "Api" == rewrite(input) with input as {"Fn::GetAtt": ["Api", "Arn"]}
  not rewrite(input) with input as {"Fn::GetAtt": ["Api", "Port"]}
  "Api" == rewrite(input) with input as {"Ref": "Api"}
}
