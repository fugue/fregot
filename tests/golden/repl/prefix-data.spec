{
    "command": "fregot",
    "arguments": ["repl", "--no-history-file", "foo.qux:prefix-data/"],
    "asserts": [
        {"exit_code": 0},
        {"stderr": "${SPEC_NAME}.stderr"},
        {"stdout": "${SPEC_NAME}.stdout"}
    ],
    "stdin": [
        ":load foo.bar:prefix-data.json",
        "data"
    ]
}
