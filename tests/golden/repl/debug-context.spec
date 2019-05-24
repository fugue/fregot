{
    "command": "fregot",
    "arguments": ["repl", "debug-context.rego"],
    "asserts": [
        {"exit_code": 0},
        {"stderr": "${SPEC_NAME}.stderr"},
        {"stdout": "${SPEC_NAME}.stdout"}
    ],
    "stdin": [
        ":break debug_context.valid_security_groups",
        ":open debug_context",
        "test_valid_security_groups",
        "security_groups"
    ]
}
