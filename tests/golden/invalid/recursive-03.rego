package recursive_03

foo {
    data.recursive_03.bar
}

bar {
    qux
}

qux {
    data.recursive_03.foo
}

