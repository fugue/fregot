{
    "command": "fregot",
    "arguments": ["repl"],
    "asserts": [
        {"exit_code": 0},
        {"stderr": "${SPEC_NAME}.stderr"},
        {"stdout": "${SPEC_NAME}.stdout"}
    ],
    "stdin": [
        ":load test.rego",
        ":open fregot.tests.repl.test",
        ":test"
    ]
}
