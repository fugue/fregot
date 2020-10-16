package fregot.tests.yaml_01

test_json_01 {
  out := yaml.unmarshal(`foo: 1`)
  out == {"foo": 1}
}

test_json_02 {
  out := yaml.marshal({"foo": 1, "bar": ["Hello", "World"]})
  out == `bar:
- Hello
- World
foo: 1
`
}
