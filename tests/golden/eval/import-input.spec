{
    "command": "fregot",
    "arguments": [
        "eval", "--input", "${SPEC_NAME}.json",
        "data.import_input.deny", "${SPEC_NAME}.rego"
    ],
    "asserts": [
        {"exit_code": 0},
        {"stderr": "${SPEC_NAME}.stderr"},
        {"stdout": "${SPEC_NAME}.stdout"}
    ]
}
