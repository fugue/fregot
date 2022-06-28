package regex_globs_match

import future.keywords.every

# Source: <https://github.com/yashtewari/glob-intersection/blob/master/non_empty_test.go>

cases_non_empty = {
	"abcd": ["abcd", "....", "[a-d]*"],
	"pqrs": [".qrs", "p.rs", "pq.s", "pqr."],
	".*": ["asdklfj", "jasdfh", "asdhfajfh", "asdflkasdfjl"],
	"d*": ["[abcd][abcd]", "d[a-z]+", ".....", "[d]*"],
	"[a-p]+": ["[p-z]+", "apapapaapapapap", ".*", "abcdefgh*"],
	"abcd[a-c]z+": ["abcd[b-d][yz]*", "abcdazzzz", "abcdbzzz", "abcdcz"],
	".*\\\\": [".*", "asdfasdf\\\\"], # Escaped \ character.
	".a.a": ["b.b.", "c.c.", "d.d.", "e.e."],
	".*.*.*.*.*.*.*.*.*.*.*.*.*.*.*": [".*.*.*.*.*.*.*.*.*.*.*"],
	"foo.*bar": ["foobar", "fooalkdsjfbar"],
}

cases_empty = {
	"abcd": ["lsdfhda", "abcdla", "asdlfk", "ksdfj"],
	"[a-d]+": ["xyz", "p+", "[e-f]+"],
	"[0-9]*": ["[a-z]", ".\\*"],
	"mamama.*": ["dadada.*", "nanana.*"],
	# ".*mamama": [".*dadada", ".*nanana"],
	".xyz.": ["paaap", ".*pqr.*"],
	"ab+": ["a", "b", "abc"],
	".*.*.*.*f": [".*.*.*.*g"],
	# ".*": [""],
}

test_regex_globs_match_non_empty {
	every lhs, rhss in cases_non_empty {
		every rhs in rhss {
			regex.globs_match(lhs, rhs)
		}
	}
}

test_regex_globs_match_empty {
	every lhs, rhss in cases_empty {
		every rhs in rhss {
			not regex.globs_match(lhs, rhs)
		}
	}
}
