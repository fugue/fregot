{
    "command": "fregot",
    "arguments": ["repl", "--no-history-file", "debug-context.rego"],
    "asserts": [
        {"exit_code": 0},
        {"stderr": "${SPEC_NAME}.stderr"},
        {"stdout": "${SPEC_NAME}.stdout"}
    ],
    "stdin": [
        ":break debug_context.valid_security_groups",
        "data.debug_context.test_valid_security_groups",
        "security_groups"
    ]
}
