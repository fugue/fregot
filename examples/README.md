# fregot examples

## demo

The [demo](./demo) example contains a Rego policy with input and test files for validating Terraform plan AMI IDs against a whitelist. [demo.rego](./demo/demo.rego) has an error that you'll debug with fregot as part of the walkthroughs. These walkthroughs use the same policy/input files and are available in two versions:

- [demo_walkthrough.md](./demo/demo_walkthrough.md): Quick version designed for presentations and demos
- [tutorial_walkthrough.md](./demo/tutorial_walkthrough.md): In-depth version designed as an introduction to fregot

## ami_id

[ami_id](./ami_id) contains an alternative version of the policy, input, and test files in [demo](./demo). This is handy for users who want to practice with fregot by introducing a Rego error and debugging it on their own.

## break_example

[break_example](./break_example) contains a very simple example policy for practicing fregot breakpoints.