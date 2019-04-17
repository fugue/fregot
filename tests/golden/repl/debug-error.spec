{
    "command": "fregot",
    "arguments": ["repl", "debug-error.rego"],
    "asserts": [
        {"exit_code": 0},
        {"stderr": "${SPEC_NAME}.stderr"},
        {"stdout": "${SPEC_NAME}.stdout"}
    ],
    "stdin": [
        ":break debug_error.test_crash",
        "data.debug_error.test_crash",
        ":stepover",
        ":stepover",
        ":where",
        "x",
        ":quit",
        ":quit"
    ]
}
