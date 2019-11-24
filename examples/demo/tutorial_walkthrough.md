# fregot + Terraform Tutorial: Interactive Debugging with fregot

## Introduction

This tutorial shows how to use fregot to debug a Rego policy. The policy checks
whether AWS EC2 instances in a Terraform plan use AMIs from an approved list.

**Note:** A quick version of this tutorial that was designed for 
presentations/demos can be accessed at 
[demo_walkthrough.md](./demo_walkthrough.md).

## Prerequisites

- `git clone` the [fregot repo](https://github.com/fugue/fregot) 
  - `git clone <SSH or HTTPS URL>` 
- Move to the `demo` directory 
  - `cd fregot/examples/demo`
- [Install 
fregot](https://github.com/fugue/fregot/blob/master/README.md#installation)

### Optional Steps

If you'd like to generate the Terraform plan JSON yourself:

- [Install Terraform v0.12 or later](https://www.terraform.io/downloads.html)
- *Optional:* [Install jq](https://stedolan.github.io/jq/download/)

## Steps

### Generate Terraform Plan as JSON

[demo.rego](demo.rego) is a Rego policy that checks AWS AMI IDs in a Terraform
plan against a whitelist. The policy contains an error that we'll debug in this
tutorial.

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
eval`](https://github.com/fugue/fregot/blob/master/README.md#fregot-eval) to
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
      "demo.rego" (line 10, column 11):
      index type error:

        10|     ami = input.resource_changes.change.after.ami
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      evalRefArg: cannot index array with a string

      Stack trace:
        rule fregot.examples.demo.amis at demo.rego:21:11
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
demo.rego:20`)

Now, evaluate the rule to activate the breakpoint:

    deny

You'll see this output:

    21|     ami = amis[ami]
            ^^^^^^^^^^^^^^^

Great! We've entered debugging mode and fregot is showing us the first line of
the rule body. Notice how the prompt shows the word `debug` now:

    fregot.examples.demo(debug)%

### Step Forward

Nothing seems out of order yet, so step forward into the next query with the
[`:step`](https://github.com/fugue/fregot#step) command:

    :step

You'll see this output:

    10|     ami = input.resource_changes.change.after.ami
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

So far, so good. Step forward again:

    :step

Look, there's the error message we saw earlier!

    (debug) error
    fregot (eval error):
      "demo.rego" (line 10, column 11):
      index type error:

        10|     ami = input.resource_changes.change.after.ami
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      evalRefArg: cannot index array with a string

      Stack trace:
        rule fregot.examples.demo.amis at demo.rego:21:11
        rule fregot.examples.demo.deny at deny:1:1

### Error Mode

fregot automatically puts you into error mode, indicated by the REPL prompt:

    fregot.examples.demo(error)%

Let's look at the error message closely. Something is wrong with the input,
since line 10 is where we assign the AMI ID in the input to the variable `ami`:

        10|     ami = input.resource_changes.change.after.ami
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Consider the error reason:

      evalRefArg: cannot index array with a string

In this case, that means the policy is referencing an array incorrectly. There's
something wrong with this syntax:

    input.resource_changes.change.after.ami

We're dealing with nested documents here, so let's start with the entire input
document and narrow down level by level until we get just the part we want,
`ami`, which is the AMI ID for each AMI in the input.

### Evaluate Expressions

We can see the entire input document by evaluating `input`:

    input

We get the expected result, which is the information in `repl_demo_input.json`.
No problems yet, so evaluate the next nested level:

    input.resource_changes

There's a lot less JSON now. Everything seems OK so far -- no errors. Narrow it
down again:

    input.resource_changes.change

Looks like we found the problem! You should see this error message:

    fregot (eval error):
      "input.resource_changes.change" (line 1, column 1):
      index type error:

        1| input.resource_changes.change
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      evalRefArg: cannot index array with a string

      Stack trace:
        rule fregot.examples.demo.amis at demo.rego:21:11
        rule fregot.examples.demo.deny at deny:1:1

### Diagnose Error

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

As you can see, `resource_changes` is an array with two items, and we should
account for this when we assign a value to the `ami` variable. We need to
iterate through `resource_changes` so we can assign _each_ `change.after.ami`
value to the `ami` variable. We can do this with `[_]`, so let's add that to the
previous expression and evaluate it again:

    input.resource_changes[_].change

Success! You should see the two items in the array. Let's narrow it all the way
down and confirm we get just the two AMI IDs:

    input.resource_changes[_].change.after.ami

The output looks great:

    = "ami-0b69ea66ff7391e80"
    = "ami-atotallyfakeamiid"

### Fix Error in Policy

Now that we know how to fix the policy, let's add `[_]` to line 10 of
`demo.rego`. It should look like this now:

    ami = input.resource_changes[_].change.after.ami

Save your changes and go back to fregot -- you'll see that the updated policy
has [automatically been reloaded](https://github.com/fugue/fregot#watch):

    Reloaded demo.rego

(Note: If you didn't launch the REPL with `--watch`, you can manually reload all
modified files with [`:reload`](https://github.com/fugue/fregot#reload).)

You're still in error mode, so [quit](https://github.com/fugue/fregot#quit)
error mode to return to normal fregot mode:

    :quit

### Enter Debug Mode Again

We should make sure the rest of the policy is evaluated correctly. Activate the
breakpoint again to return to debug mode:

    deny

You'll see the first query of `deny` again:

    21|     ami = amis[ami]
            ^^^^^^^^^^^^^^^

Instead of stepping _into_ the next query, we'll step _over_ it with the
[`:next`](https://github.com/fugue/fregot#next) command:

    :next

This brings us to line 22 of `demo.rego`, and you'll see this output, where the
`invalid_ami` function determines whether the first AMI ID is whitelisted:

    22|     invalid_ami(ami)
            ^^^^^^^^^^^^^^^^

Let's step over the next couple queries until we see the final results of the
`deny` evaluation:

    :next

You'll see this output, as fregot evaluates the second AMI ID for validity:

    22|     invalid_ami(ami)
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

    Reloaded repl_demo_input.json {}

The empty braces mean that `deny` does not return `true`. The Terraform plan
input has passed validation because both AMI IDs are on the whiteliste.

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

    Reloaded demo.rego {}

Now that we've changed the whitelist, the Terraform plan passes validation --
hooray! Everything works as expected, thanks to a little help from fregot.

### Evaluate the Terraform Plan

Since you've successfully debugged the policy, you can exit fregot:

    :quit

You may use the same policy to evaluate other JSON Terraform plans by executing
the same `fregot eval` command from earlier:

    fregot eval --input \
      repl_demo_input.json \
      'data.fregot.examples.demo.deny' demo.rego

(Note: fregot also works with YAML input!)

If the AMIs in the Terraform plan are whitelisted and the plan passes
validation, you'll see output like this because `deny` does not return true:

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