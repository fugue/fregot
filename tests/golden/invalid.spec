{
    "input_files": "invalid-*.rego",
    "command": "fregot",
    "arguments": ["test", "${SPEC_INPUT_FILE}"],
    "asserts": [
        { "exit_code": 1
        },
        {
            "stderr": "${SPEC_INPUT_FILE}.stderr"
        }
    ]
}
