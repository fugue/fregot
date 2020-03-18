package fregot.tests.with_03

person = {
  "name": "Tom",
  "age": 34
}

the_age = ret {
  ret = person.age
}

test_with_data {
  age1 = the_age with data.fregot.tests.with_03.person as {"age": 1}
  age1 == 1

  # This does not work with stock `opa` but does work with `fregot`.
  age2 = the_age with data.fregot.tests.with_03.person.age as 2
  age2 == 2

  age3 = the_age with data.fregot.tests.with_03.person as {"name": 1, "age": 3}
  age3 == 3
}
