{
    "command": "fregot",
    "arguments": ["repl", "--no-history-file", "--input", "input.json"],
    "asserts": [
        {"exit_code": 0},
        {"stderr": "${SPEC_NAME}.stderr"},
        {"stdout": "${SPEC_NAME}.stdout"}
    ],
    "stdin": [
        "input.message"
    ]
}
