package every_in_01

import future.keywords.every

good_tags = {
	"stage": "dev",
	"owner": "dragon",
}

bad_tags_1 = {
	"stage": "dev",
	"owner": "Dragon",
}

bad_tags_2 = {
	"Stage": "dev",
	"owner": "dragon",
}

valid_tags(tags) {
	every k, v in tags {
		lower(k) == k
		lower(v) == v
	}
}

test_tags {
	valid_tags(good_tags)
	not valid_tags(bad_tags_1)
	not valid_tags(bad_tags_2)
}
