{
    "command": "fregot",
    "arguments": ["repl"],
    "asserts": [
        {"exit_code": 0},
        {"stderr": "${SPEC_NAME}.stderr"},
        {"stdout": "${SPEC_NAME}.stdout"}
    ],
    "stdin": [
        ":open mypkg1",
        "r = 1",
        "r",
        ":open data.mypkg2",
        "r = 2",
        "r",
        ":open data.mypkg1",
        "r",
        "data.mypkg2.r"
    ]
}
