package fregot.examples.ami_id

import data.fregot.examples.ami_id.inputs

test_valid_single_ami {
    allow with input as inputs.valid_single_ami
}

test_invalid_single_ami {
    not allow with input as inputs.invalid_single_ami
}

test_valid_two_different_amis {
    allow with input as inputs.valid_two_different_amis
}

test_invalid_two_different_amis {
    not allow with input as inputs.invalid_two_different_amis
}

test_valid_two_same_amis {
    allow with input as inputs.valid_two_same_amis
}

test_invalid_two_same_amis {
    not allow with input as inputs.invalid_two_same_amis
}

test_valid_single_ami_from_tf_plan {
    allow with input as inputs.valid_single_ami_from_tf_plan
}

test_invalid_single_ami_from_tf_plan {
    not allow with input as inputs.invalid_single_ami_from_tf_plan
}