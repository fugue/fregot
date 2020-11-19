package fregot.tests.yaml_01

test_yaml_01 {
  out := yaml.unmarshal(`foo: 1`)
  out == {"foo": 1}
}

test_yaml_02 {
  out := yaml.marshal({"foo": 1, "bar": ["Hello", "World"]})
  out == `bar:
- Hello
- World
foo: 1
`
}

test_yaml_03 {
  yaml.is_valid("foo: 1")
  not yaml.is_valid("foo: [")
}
