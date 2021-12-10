# fregot examples

## demo

The [demo](./demo) example contains a Rego policy with input and test files for validating Terraform plan AMI IDs against a whitelist. [demo.rego](./demo/demo.rego) has an error that you'll debug with fregot as part of the walkthrough in [tutorial_walkthrough.md](./demo/tutorial_walkthrough.md/].

## ami_id

[ami_id](./ami_id) contains an alternative version of the policy, input, and test files in [demo](./demo). This is handy for users who want to practice with fregot by introducing a Rego error and debugging it on their own.

## break_example

[break_example](./break_example) contains a very simple example policy for practicing fregot breakpoints.