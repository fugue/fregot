{
    "command": "fregot",
    "arguments": ["repl", "--no-history-file", "set.rego"],
    "asserts": [
        {"exit_code": 0},
        {"stderr": "${SPEC_NAME}.stderr"},
        {"stdout": "${SPEC_NAME}.stdout"}
    ],
    "stdin": ["data.set.numbers[i]"]
}
