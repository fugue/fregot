{
    "command": "fregot",
    "arguments": ["repl", "step.rego"],
    "asserts": [
        {"exit_code": 0},
        {"stderr": "${SPEC_NAME}.stderr"},
        {"stdout": "${SPEC_NAME}.stdout"}
    ],
    "stdin": [
        ":break step.test_step",
        "data.step.test_step",
        ":next",
        ":next"
    ]
}
