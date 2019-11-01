{
    "input_files": "invalid_*.rego",
    "command": "fregot",
    "arguments": ["test", "${SPEC_INPUT_FILE}"],
    "asserts": [
        { "exit_code": 1
        },
        {
            "stderr": "${SPEC_INPUT_NAME}.stderr"
        },
        {
            "stdout": "${SPEC_INPUT_NAME}.stdout"
        }
    ]
}
