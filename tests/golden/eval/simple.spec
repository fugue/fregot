{
    "command": "fregot",
    "arguments": [
        "eval", "--input", "simple.json", "data.simple.double", "simple.rego"
    ],
    "asserts": [
        {"exit_code": 0},
        {"stderr": "${SPEC_NAME}.stderr"},
        {"stdout": "${SPEC_NAME}.stdout"}
    ]
}
