{
    "command": "fregot",
    "arguments": ["repl"],
    "stdin": [
        ":l rules.rego",
        ":type some_number",
        ":type some_set",
        ":type some_object"
    ],
    "asserts": [
        {"stdout": "${SPEC_NAME}.stdout"},
        {"stderr": "${SPEC_NAME}.stderr"},
        {"exit_code": 0}
    ]
}
