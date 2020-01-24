{
    "command": "fregot",
    "arguments": ["repl", "--no-history-file"],
    "stdin": [
        ":l granular.rego",
        ":type people",
        ":type people[99].address"
    ],
    "asserts": [
        {"stdout": "${SPEC_NAME}.stdout"},
        {"stderr": "${SPEC_NAME}.stderr"},
        {"exit_code": 0}
    ]
}
