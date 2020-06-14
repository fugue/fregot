{
    "command": "fregot",
    "arguments": ["repl", "--no-history-file"],
    "asserts": [
        {"exit_code": 0},
        {"stderr": "${SPEC_NAME}.stderr"},
        {"stdout": "${SPEC_NAME}.stdout"}
    ],
    "stdin": [
        ":load data.yaml",
        "data.yaml_policy.filename"
    ]
}