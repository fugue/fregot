package types_03

test_union {
  datas := {
      {"number": 1},
      1
  }
  d := datas[_]
  d.number == 1
}
