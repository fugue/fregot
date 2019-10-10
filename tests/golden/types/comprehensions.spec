{
    "command": "fregot",
    "arguments": ["repl"],
    "stdin": [
        ":type [x | nums = [1, 2, 3]; x = nums[_]]"
    ],
    "asserts": [
        {"stdout": "${SPEC_NAME}.stdout"},
        {"stderr": "${SPEC_NAME}.stderr"},
        {"exit_code": 0}
    ]
}
