# Here are some tests that cover dealing with sets and json.patch, since that
# is not covered by the spec.
package json_patch_cases.set_tests
cases = [
  {
    "comment": "using set",
    "doc":  {"foo": {"a", "b", "c"}},
    "patch": [{"op": "remove", "path": "foo/b"}],
    "expected": {"foo": {"a", "c"}}
  },
  {
    "comment": "using set",
    "doc": {"foo": {"a", "b", "c"}},
    "patch": [{"op": "add", "path": "foo/d", "value": "d"}],
    "expected": {"foo": {"a", "b", "c", "d"}}
  },
  {
    "comment": "using set",
    "doc":  {"foo": {"a", "b", "c"}},
    "patch": [{"op": "add", "path": "foo/d", "value": "e"}],
    "error": "value does not match key"
  },
  {
    "comment": "using set",
    "doc": {"foo": {"a", "b"}, "bar": {"c", "d"}},
    "patch": [{"op": "move", "from": "foo/a", "path": "bar/a"}],
    "expected": {"foo": {"b"}, "bar": {"c", "d", "a"}}
  },
  {
    "comment": "using set",
    "doc": {[1]},
    "patch": [{"op": "add", "path": [[1], 1], "value": 2}],
    "expected": {[1, 2]}
  }
]
