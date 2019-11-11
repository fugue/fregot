{
    "command": "bash",
    "arguments": ["${SPEC_NAME}.sh"],
    "asserts": [
        {"exit_code": 0},
        {"stderr": "${SPEC_NAME}.stderr"},
        {"stdout": "${SPEC_NAME}.stdout"}
    ]
}
