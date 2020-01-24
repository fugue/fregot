{
    "command": "fregot",
    "arguments": ["repl", "--no-history-file"],
    "stdin": [
        ":type [x | nums = [1, 2, 3]; x = nums[_]]",
        ":type {x | nums = [1, 2, 3]; x = nums[_]}",
        ":type {k: v | nums = [1, 2, 3]; strs = ['one', 'two', 'three']; v = nums[_]; k = strs[_]}"
    ],
    "asserts": [
        {"stdout": "${SPEC_NAME}.stdout"},
        {"stderr": "${SPEC_NAME}.stderr"},
        {"exit_code": 0}
    ]
}
