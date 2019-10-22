{
    "command": "fregot",
    "arguments": ["repl"],
    "asserts": [
        {"exit_code": 0},
        {"stderr": "${SPEC_NAME}.stderr"},
        {"stdout": "${SPEC_NAME}.stdout"}
    ],
    "stdin": [
        ":load debug-cache.rego",
        "test_allow",
        ":break test_allow",
        "test_allow",
        ":continue"
    ]
}
