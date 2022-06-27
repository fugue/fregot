package every_in_01

import future.keywords.every

numbers := [1, 2, 3, 4]

odd_numbers := [n | n := numbers[_]; n % 2 == 1]

test_odd_numbers {
	every n in odd_numbers {
		n % 2 == 1
	}
}

fail_numbers {
	every n in numbers {
		n % 2 == 1
	}
}

test_fail_numbers {
	not fail_numbers
}
