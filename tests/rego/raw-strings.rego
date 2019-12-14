package fregot.tests.raw_strings

test_raw_strings_01 {
    "\"hello\"" == `"hello"`
    `^([A-Z][A-Z0-9\-]+)+` == "^([A-Z][A-Z0-9\\-]+)+"
}

test_raw_strings_02 {
    `some
string with
newlines
` == "some\nstring with\nnewlines\n"
}
