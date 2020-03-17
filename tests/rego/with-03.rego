package fregot.tests.with_03

data_age = ret {
  ret = data.person.age
}

test_data {
  age = data_age with data.person as {"age": 34}
  age == 34
}
