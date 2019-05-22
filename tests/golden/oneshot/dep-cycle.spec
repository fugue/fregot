{
    "command": "fregot",
    "arguments": [
        "test",
        "dep-cycle-01.rego",
        "dep-cycle-02.rego",
        "dep-cycle-03.rego"
    ],
    "asserts": [
        {
            "exit_code": 1
        },
        {
            "stderr": "${SPEC_NAME}.stderr"
        }
    ]
}
