{
    "command": "fregot",
    "arguments": ["repl", "input-doc.rego"],
    "asserts": [
        {"exit_code": 0},
        {"stderr": "${SPEC_NAME}.stderr"},
        {"stdout": "${SPEC_NAME}.stdout"}
    ],
    "stdin": [
        ":input input-doc.json",
        ":open fregot.tests.repl.input_doc",
        "security_groups"
    ]
}
