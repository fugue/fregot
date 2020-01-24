{
    "command": "fregot",
    "arguments": ["repl", "--no-history-file", "rewind.rego"],
    "asserts": [
        {"exit_code": 0},
        {"stderr": "${SPEC_NAME}.stderr"},
        {"stdout": "${SPEC_NAME}.stdout"}
    ],
    "stdin": [
        ":break rewind.crashing_rule",
        "data.rewind.crashing_rule",
        ":next",
        ":next",
        ":rewind",
        ":quit",
        ":break rewind.failing_rule",
        "data.rewind.failing_rule",
        ":next",
        ":next",
        ":rewind",
        ":quit"
    ]
}
