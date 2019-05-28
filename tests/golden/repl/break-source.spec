{
    "command": "fregot",
    "arguments": ["repl", "step.rego"],
    "asserts": [
        {"exit_code": 0},
        {"stderr": "${SPEC_NAME}.stderr"},
        {"stdout": "${SPEC_NAME}.stdout"}
    ],
    "stdin": [
        ":break step.rego:5",
        "data.step.test_step",
        ":continue"
    ]
}
