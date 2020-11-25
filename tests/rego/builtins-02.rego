# Aggregates
package fregot.tests.builtins_02

test_array_concat {
  array.concat([], []) == []
  array.concat(["hey"], [1]) == ["hey", 1]
}

test_array_slice {
  array.slice([], 0, 1) == []
  array.slice(["hey"], 2, 1) == []
  array.slice(["hey"], 0, 1) == ["hey"]
  array.slice(["hey"], -1, 2) == ["hey"]
  array.slice(["hey", "ho"], 1, 2) == ["ho"]
  array.slice(["hey", "ho"], 1, 10) == ["ho"]
  array.slice(["hey", "ho", "let's", "go"], 1, 3) == ["ho", "let's"]
}

test_format_int {
  format_int(100, 2) == "1100100"
}

test_indexof {
  indexof("hello world", "w") == 6
  indexof("hello world", "a") == 0 - 1  # TODO(jaspervdj): negative literals
  indexof("hello world", "") == 0
}

test_max {
  max([1, 2]) == 2
  not max([])
}

test_min {
  min([1, 2]) == 1
  not min([])
}

test_product {
  product([]) == 1
  product([1, 2, 3, 4, 5]) == 120
  product([0.5, 0.5]) == 0.25
}

test_sort {
  sort([1, true, "a", [], null]) == [null, true, 1, "a", []]
  sort(["a", "b", "c"]) == ["a", "b", "c"]
  sort([0.5, 1, 1.5]) == [0.5, 1, 1.5]
}

test_sum {
  sum([]) == 0
  sum([1, 1]) == 2
  sum([1.2, 1]) == 2.2
  sum([0.5, 0.5, 0.5]) == 1.5
}
