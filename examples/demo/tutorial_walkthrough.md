# fregot + Terraform Tutorial: Interactive Debugging with fregot

## Introduction

This tutorial shows how to use fregot to debug a Rego policy. The policy checks
whether AWS EC2 instances in a Terraform plan use AMIs from an approved list.

## Prerequisites

- `git clone https://github.com/fugue/fregot.git` 
- `cd fregot/examples/demo`
- [Install 
fregot](https://github.com/fugue/fregot/blob/main/README.md#installation)

### Optional Steps

If you'd like to generate the Terraform plan JSON yourself:

- [Install Terraform v0.12 or later](https://www.terraform.io/downloads.html)
- *Optional:* [Install jq](https://stedolan.github.io/jq/download/)

## Steps

### Generate Terraform Plan as JSON

Let's say your organization requires Amazon Web Services EC2 instances to
_only_ use hardened Linux Amazon Machine Images (AMIs) that are on a whitelist. 
Your boss wants to prevent any Terraform with a non-blessed AMI ID from being 
deployed, so you've installed fregot and have written a Rego policy to validate 
the Terraform plan before it is applied. You'll be working with these files:

[demo.rego](demo.rego) is a Rego policy that checks AWS AMI IDs in a Terraform plan against a whitelist of hardened images. The policy contains an error that we'll debug in this tutorial.

[demo.tf](demo.tf) is a Terraform file that will deploy two EC2 instances. If
you take a look, you'll see that `ami-0b69ea66ff7391e80` is listed in
`approved_amis` in the policy [demo.rego](demo.rego), and
`ami-atotallyfakeamiid` is (unsurprisingly) not.

[repl_demo_input.json](repl_demo_input.json) is the Terraform plan formatted as
JSON so fregot can evaluate it. We've done this for you, but if you've installed
[Terraform v0.12 or later](https://www.terraform.io/downloads.html) and you'd
like to generate the output yourself, you can do so with the following commands:

[Initialize Terraform
directory:](https://www.terraform.io/docs/commands/init.html)

    terraform init

[Create Terraform plan:](https://www.terraform.io/docs/commands/plan.html)

    terraform plan -out=tfplan

[Generate JSON representation of plan and pretty-print it with
jq:](https://www.terraform.io/docs/commands/show.html)

    terraform show -json tfplan | jq . > repl_demo_input.json

### Evaluate Terraform Plan with Fregot

Let's start by validating the Terraform plan JSON against the Rego policy. We'll
use [`fregot
eval`](https://github.com/fugue/fregot/blob/main/README.md#fregot-eval) to
specify the input file (`repl_demo_input.json`), the function we want to
evaluate (`data.fregot.examples.demo.deny`), and the Rego file it is in
(`demo.rego`):

    fregot eval --input \
      repl_demo_input.json \
      'data.fregot.examples.demo.deny' demo.rego

But wait, what's this...

### Oh No, an Error!

Uh oh! There's an error in the Rego file. Evaluating `deny` produces this
message:

    fregot (eval error):
      "demo.rego" (line 11, column 5):
      builtin error:

        11|     startswith(ami, "ami-")
                ^^^^^^^^^^^^^^^^^^^^^^^

      Expected string but got object

      Stack trace:
        rule fregot.examples.demo.amis at demo.rego:22:11
        rule fregot.examples.demo.deny at cli:1:1

Well, it's a good thing we just installed fregot! Let's use the REPL to
interactively debug the code.

### Launch REPL

We'll start by [launching the
REPL](https://github.com/fugue/fregot#fregot-repl):

    fregot repl demo.rego --watch

The [`--watch`](https://github.com/fugue/fregot#watch) flag tells fregot to
automatically reload the loaded files (including input) when it detects a
change.

(Handy, right? We'll take the `watch` feature one step further in a little
while!)

### Load Policy and Input

First, [load the policy](https://github.com/fugue/fregot#load):

    :load demo.rego

Next, [set the input](https://github.com/fugue/fregot#input):

    :input repl_demo_input.json

We're going to take a closer look at `data.fregot.examples.demo.deny`. Let's set
a breakpoint so we can investigate.

### Set and Activate Breakpoint

To set the breakpoint at `deny`, you can use the
[`:break`](https://github.com/fugue/fregot#break) command with the rule name.
Since the policy has already been loaded in the REPL, rather than using the full
name `data.fregot.examples.demo.deny`, you can simplify it like so:

    :break deny

(We set the breakpoint with the rule name here, but we could also have [used the
line number](https://github.com/fugue/fregot#step-1-set-breakpoint): `:break
demo.rego:21`)

Now, evaluate the rule to activate the breakpoint:

    deny

You'll see this output:

    22|     ami = amis[ami]
            ^^^^^^^^^^^^^^^

Great! We've entered debugging mode and fregot is showing us the first line of
the rule body. Notice how the prompt shows the word `debug` now:

    fregot.examples.demo(debug)%

### Step Forward

Nothing seems out of order yet, so step forward into the next query with the
[`:step`](https://github.com/fugue/fregot#step) command:

    :step

You'll see this output:

    10|     ami = input.resource_changes[_].change.after
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

So far, so good. Step forward again:

    :step

You'll see this output:

    11|     startswith(ami, "ami-")
            ^^^^^^^^^^^^^^^^^^^^^^^

Again, still looking good. Step forward one more time:

    :step

Look, there's the error message we saw earlier!

    (debug) error
    fregot (eval error):
      "demo.rego" (line 11, column 5):
      builtin error:

        11|     startswith(ami, "ami-")
                ^^^^^^^^^^^^^^^^^^^^^^^

      Expected string but got object

      Stack trace:
        rule fregot.examples.demo.amis at demo.rego:22:11
        rule fregot.examples.demo.deny at deny:1:1

### Error Mode

fregot automatically puts you into error mode, indicated by the REPL prompt:

    fregot.examples.demo(error)%

Let's look at the error message closely. Something is wrong with the value of the `ami` variable,
since line 11 is where we check whether it starts with the string `"ami-"`:

        11|     startswith(ami, "ami-")
                ^^^^^^^^^^^^^^^^^^^^^^^

Consider the error reason:

      Expected string but got object

In this case, that means the value of `ami` isn't a string like we expect it to be. There's
something wrong with this syntax on line 10:

    ami = input.resource_changes[_].change.after

Let's see for ourselves what the value of `ami` looks like in the REPL, so we can figure out how to fix our syntax.

### Check Type

We can use the [`:type`](https://github.com/fugue/fregot#type) command in the REPL to return the type of a term in the loaded package. We expect the `ami` variable to represent a string -- the AMI ID -- so let's find out what it actually is:

    :type ami

We see this output:

    ami : object{
      "instance_initiated_shutdown_behavior": null,
      "ami": string,
      "ebs_optimized": null,
      "instance_type": string,
      "user_data": null,
      "monitoring": null,
      "tags": null,
      "get_password_data": boolean,
      "credit_specification": array{},
      "disable_api_termination": null,
      "timeouts": null,
      "source_dest_check": boolean,
      "user_data_base64": null,
      "iam_instance_profile": null
    }

Aha! This is definitely not just the AMI ID. The `ami` variable currently represents an object, which includes a host of other information.

We need to extract just the `"ami"` string inside the object. This means that the way we've assigned the `ami` variable is incorrect. As a refresher, this is the Rego code on line 10:

    ami = input.resource_changes[_].change.after

Looks like we need to go one level deeper in the input document. If we add `.ami` at the end, it should narrow down the input to just the `"ami"` string.

But before we make any code changes, let's examine the input document and make sure that's the correct syntax.

### Examine Input

First, quit debug mode:

    :quit

Now can view the entire input by evaluating `input`:

    input

That's a lot of JSON! Let's narrow it down all the way to `input.resource_changes[_].change.after.ami`:

    input.resource_changes[_].change.after.ami

We see this output:

    (debug) = "ami-0b69ea66ff7391e80"
    (debug) = "ami-atotallyfakeamiid"
    (debug) finished

Yes! That is the syntax we want to use to declare the `ami` variable on line 10. As you can see, it returns just the AMI ID strings we need.

If you look at `repl_demo_input.json`, you'll see the JSON follows this basic
structure:

    {
      "resource_changes": [
        {
          "change": {
            "after": {
              "ami": "ami-0b69ea66ff7391e80"
            },
          }
        },
        {
          "change": {
            "after": {
              "ami": "ami-atotallyfakeamiid"
            }
          }
        }
      ]
    }

As you can see, we simply forgot to narrow down the input to the `ami` field.

### Fix Error in Policy

Now that we know how to fix the policy, let's add `.ami` to line 10 of
`demo.rego`. It should look like this now:

    ami = input.resource_changes[_].change.after.ami

Save your changes and go back to fregot -- you'll see that the updated policy
has [automatically been reloaded](https://github.com/fugue/fregot#watch):

    Reloaded demo.rego

(Note: If you didn't launch the REPL with `--watch`, you can manually reload all
modified files with [`:reload`](https://github.com/fugue/fregot#reload).)

### Enter Debug Mode Again

We should make sure the rest of the policy is evaluated correctly. Activate the
breakpoint again to return to debug mode:

    deny

You'll see the first query of `deny` again:

    22|     ami = amis[ami]
            ^^^^^^^^^^^^^^^

Instead of stepping _into_ the next query, we'll step _over_ it with the
[`:next`](https://github.com/fugue/fregot#next) command:

    :next

This brings us to line 23 of `demo.rego`, and you'll see this output, where the
`invalid_ami` function determines whether the first AMI ID is whitelisted:

    23|     invalid_ami(ami)
            ^^^^^^^^^^^^^^^^

Let's step over the next couple queries until we see the final results of the
`deny` evaluation:

    :next

You'll see this output, as fregot evaluates the second AMI ID for validity:

    23|     invalid_ami(ami)
            ^^^^^^^^^^^^^^^^

Step over the next query again:

    :next

You'll see this output:

    (debug) = true
    (debug) finished

This means fregot has finished evaluating the input. `deny` returns true because
`ami-atotallyfakeamiid` is not whitelisted -- exactly as expected!

fregot has automatically exited debugging mode. Now, quit the REPL:

    :quit

Let's evaluate the policy as we did before -- this time, it should work
properly:

    fregot eval --input \
      repl_demo_input.json \
      'data.fregot.examples.demo.deny' demo.rego

And it does!

    [true]

`deny` returns `true`, which means we've successfully tested the Terraform plan
against the Rego policy and determined that the plan fails validation.

But why stop there? Let's make some changes and use the
[`:watch`](https://github.com/fugue/fregot#watch) command to automatically print
the updated output.

### Launch REPL Again

Jump into the REPL again using the `--watch` flag:

    fregot repl demo.rego --watch

Load the policy:

    :load demo.rego

Set the input:

    :input repl_demo_input.json

### Set Watch on `deny`

This time, use `:watch` to automatically re-evaluate `deny` any time we modify
either the policy or input:

    :watch deny

We're going to modify the input to prove that `deny` works as expected. In
`repl_demo_input.json`, replace `ami-atotallyfakeamiid` with
`ami-04b9e92b5572fa0d1` on lines 39, 131, and 217 and save your changes. fregot
automatically reloads the input and outputs the evaluation, so you'll see this:

    Reloaded repl_demo_input.json 
    {}

The empty braces means that `deny` returns no results -- concretely meaning
that the request will not be denied. The Terraform plan input has passed
validation because both AMI IDs are on the whitelist.

Back in the input, undo the change so `ami-04b9e92b5572fa0d1` is replaced with
`ami-atotallyfakeamiid`, then save again. fregot produces this output:

    Reloaded repl_demo_input.json
    = true

The Terraform plan now fails validation, as it should.

### Update Policy

You can even change the policy file and fregot automatically reloads it, too,
and prints the updated evaluation.

On line 5 of `demo.rego`, change `ami-04b9e92b5572fa0d1` to
`ami-atotallyfakeamiid`. Now line 5 looks like this:

    "ami-atotallyfakeamiid", "ami-0b69ea66ff7391e80"

Save the file. fregot reloads the policy and prints the updated evaluation:

    Reloaded demo.rego 
    {}

Now that we've changed the whitelist, the Terraform plan passes validation --
hooray! Everything works as expected, thanks to a little help from fregot.

### Evaluate the Terraform Plan

Since you've successfully debugged the policy, you can exit fregot:

    :quit

You may use the same policy to evaluate other JSON Terraform plans by executing
the same `fregot eval` command from earlier, substituting `your_input_file_here` for your own input:

    fregot eval --input \
      your_input_file_here \
      'data.fregot.examples.demo.deny' demo.rego

(Note: fregot also works with YAML input!)

If the AMIs in the Terraform plan are whitelisted and the plan passes
validation, you'll see output like this because `deny` returns no results:

    []

If the AMI IDs in the plan aren't whitelisted, the output looks like this
because `deny` returns true, as you saw earlier:

    [true]

You can check any other Terraform plan for whitelisted AMIs by outputting the
plan as JSON and evaluating it with fregot and the `demo.rego` policy.

## What's Next?

If you want to try your hand at debugging on your own, check out an alternative
version of this policy at [fregot/examples/ami_id/](../ami_id), where you can
introduce an error and debug with fregot to your heart's content.
