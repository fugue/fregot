# Walkthrough

The policy `examples/ami_id/ami_id_debug.rego` is supposed to check AMI IDs in a Terraform plan against an AMI whitelist, but we've introduced an index type error in line 10.

SHOW TERRAFORM FILE:

```
cat examples/ami_id/ami_debug.tf
```

SHOW POLICY -- OR JUST OPEN IT UP IN YOUR TEXT EDITOR:

```
cat examples/ami_id/ami_id_debug.rego
```

WE ALREADY CREATED THE TERRAFORM PLAN AS JSON, WHICH WE'LL USE AS INPUT. SHOW INPUT OR OPEN IN TEXT EDITOR:

```
cat examples/ami_id/repl-debug-input.json
```

CHECK OUT LINE 39 -- IT'S NOT A REAL AMI ID. THIS PLAN SHOULD FAIL THE POLICY. LET'S EVALUATE IT:

```
fregot eval --input \
  examples/ami_id/repl-debug-input.json  \
  'data.fregot.examples.ami_id_debug.deny' \
  examples/ami_id/ami_id_debug.rego
```

OOPS, AN ERROR MESSAGE. YOU'LL SEE THIS OUTPUT:

```
fregot (eval error):
  "examples/ami_id/ami_id_debug.rego" (line 10, column 11):
  index type error:

    10|     ami = input.resource_changes.change.after.ami
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  evalRefArg: cannot index array with a string

  Stack trace:
    rule fregot.examples.ami_id_debug.amis at examples/ami_id/ami_id_debug.rego:21:11
    rule fregot.examples.ami_id_debug.deny at cli:1:1
```

TIME TO DEBUG IT WITH FREGOT'S INTERACTIVE DEBUGGING MODE. LAUNCH REPL:

```
fregot repl examples/ami_id/ami_id_debug.rego --watch
```

LOAD POLICY:

```
:load examples/ami_id/ami_id_debug.rego
```

LOAD INPUT:

```
:input examples/ami_id/repl-debug-input.json
```

EVALUATE `deny`:

```
deny
```

YOU'LL SEE THIS OUTPUT:

```
fregot (eval error):
  "examples/ami_id/ami_id_debug.rego" (line 11, column 11):
  index type error:

    11|     ami = input.resource_changes.change.after.ami
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  evalRefArg: cannot index array with a string

  Stack trace:
    rule fregot.examples.ami_id_debug.amis at examples/ami_id/ami_id_debug.rego:29:11
    rule fregot.examples.ami_id_debug.deny at deny:1:1
```

YUP, THAT'S THE ERROR MESSAGE WE SAW. SET BREAKPOINT:

```
:break deny
```

ACTIVATE BREAKPOINT TO ENTER DEBUG MODE:

```
deny
```

YOU'LL SEE THIS OUTPUT:

```
29|     ami = amis[ami]
        ^^^^^^^^^^^^^^^
```

STEP FORWARD TO GET TO THE BROKEN PART:

```
:step
```

YOU'LL SEE THIS OUTPUT:

```
11|     ami = input.resource_changes.change.after.ami
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

SO FAR, SO GOOD. STEP AGAIN:

```
:step
```

YOU'LL SEE THIS OUTPUT:

```
(debug) error
fregot (eval error):
  "examples/ami_id/ami_id_debug.rego" (line 11, column 11):
  index type error:

    11|     ami = input.resource_changes.change.after.ami
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  evalRefArg: cannot index array with a string

  Stack trace:
    rule fregot.examples.ami_id_debug.amis at examples/ami_id/ami_id_debug.rego:29:11
    rule fregot.examples.ami_id_debug.deny at deny:1:1
```

SOMETHING'S WRONG WITH THE INPUT. FREGOT PUTS YOU IN ERROR MODE AUTOMATICALLY. LET'S SEE THE WHOLE INPUT AND DRILL DOWN TO THE PART WE WANT:

```
input
```

THE OUTPUT IS A LOT OF JSON. SO FAR, SO GOOD. NARROW THE INPUT DOWN TO THE NEXT PART:

```
input.resource_changes
```

THE OUTPUT IS SLIGHTLY LESS JSON -- GREAT. NARROW DOWN TO THE NEXT PART:

```
input.resource_changes.change
```

YOU'LL SEE AN ERROR AGAIN IN THIS OUTPUT:

```
fregot (eval error):
  "input.resource_changes.change" (line 1, column 1):
  index type error:

    1| input.resource_changes.change
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  evalRefArg: cannot index array with a string

  Stack trace:
    rule fregot.examples.ami_id_debug.amis at examples/ami_id/ami_id_debug.rego:29:11
    rule fregot.examples.ami_id_debug.deny at deny:1:1
```

WELL THERE'S YER PROBLEM. `resource_changes` IS AN ARRAY. LET'S FIX IT BY ADDING `[_]`:

```
input.resource_changes[_].change
```

THE OUTPUT IS A BUNCH OF JSON AGAIN AND IT LOOKS GREAT. VERIFY THAT THE INPUT LOOKS RIGHT WHEN YOU DRILL ALL THE WAY DOWN:

```
input.resource_changes[_].change.after.ami
```

GREAT, WE'RE LEFT WITH JUST THE TWO AMI IDS. NOW LET'S FIX THE POLICY. CHANGE `resource_changes` to `resource_changes[_]` in `ami_id_debug.rego`. LINE 10 SHOULD LOOK LIKE THIS NOW:

```
    ami = input.resource_changes[_].change.after.ami
```

SAVE IT AND FREGOT AUTO-MAGICALLY RELOADS THE POLICY -- YOU'LL SEE THIS OUTPUT:

```
Reloaded examples/ami_id/ami_id_debug.rego
```

QUIT DEBUG MODE:

```
:quit
```

ACTIVATE THE BREAKPOINT TO ENTER DEBUG MODE AGAIN TO MAKE SURE THE REST OF THE POLICY IS EVALUATED CORRECTLY:

```
deny
```

YOU'LL SEE THIS OUTPUT:

```
21|     ami = amis[ami]
        ^^^^^^^^^^^^^^^
```

STEP OVER TO THE NEXT FUNCTION:

```
:next
```

YOU'LL SEE THIS OUTPUT, WHICH MEANS FREGOT HAS SUCCESSFULLY ADVANCED TO THE NEXT FUNCTION:

```
22|     invalid_ami(ami)
        ^^^^^^^^^^^^^^^^
```

LET'S STEP OVER THE NEXT COUPLE FUNCTIONS UNTIL WE SEE THE RESULTS OF THE EVALUATION:

```
:next
```

YOU'LL SEE THIS OUTPUT:

```
22|     invalid_ami(ami)
        ^^^^^^^^^^^^^^^^
```

LOOKS GOOD. DO IT AGAIN AGAIN:

```
:next
```

YOU'LL SEE THIS OUTPUT:

```
(debug) = true
(debug) finished
```

GREAT! IT WORKED AND YOU'VE AUTOMATICALLY EXITED DEBUGGING MODE. QUIT FREGOT.

```
:quit
```

LET'S EVALUATE THE POLICY AGAIN:

```
fregot eval --input \
  examples/ami_id/repl-debug-input.json  \
  'data.fregot.examples.ami_id_debug.deny' \
  examples/ami_id/ami_id_debug.rego
```

THIS TIME YOU GET THE RIGHT ANSWER:

```
[true]
```

LET'S CHANGE THE INPUT AND PROVE IT'S WORKING. ENTER THE REPL AGAIN:

```
fregot repl examples/ami_id/ami_id_debug.rego --watch
```

LOAD POLICY:

```
:load examples/ami_id/ami_id_debug.rego
```

LOAD INPUT:

```
:input examples/ami_id/repl-debug-input.json
```

SET A WATCH ON `deny` SO IT IS AUTOMATICALLY RE-EVALUATED WHEN WE CHANGE THE INPUT:

```
:watch deny
```

IN `repl-debug-input.json`, REPLACE `ami-atotallyfakeamiid` WITH `ami-04b9e92b5572fa0d1` AND SAVE. FREGOT AUTOMATICALLY RELOADS THE INPUT AND OUTPUTS THE EVALUATION, SO YOU'LL SEE THIS:

```
Reloaded examples/ami_id/repl-debug-input.json
{}
```

THIS MEANS THE TERRAFORM PLAN INPUT HAS PASSED VALIDATION BECAUSE THE AMI IDs ARE ON THE WHITELIST. IN THE INPUT, UNDO THE CHANGE SO `ami-04b9e92b5572fa0d1` IS REPLACED WITH `ami-atotallyfakeamiid` AND SAVE AGAIN. FREGOT RELOADS THE INPUT AND PRINTS THE UPDATED EVALUATION. YOU'LL SEE THIS:

```
Reloaded examples/ami_id/repl-debug-input.json
= true
```

THE TERRAFORM PLAN INPUT NOW FAILS VALIDATION, AS IT SHOULD. IF YOU LIKE, YOU CAN EVEN CHANGE THE POLICY FILE AND FREGOT WILL AUTOMATICALLY RELOAD THAT, TOO, AND PRINT THE UPDATED EVALUATION. IN `ami_id_debug.rego` ON LINE 5, CHANGE `ami-04b9e92b5572fa0d1` TO `ami-atotallyfakeamiid`. NOW LINE 5 LOOKS LIKE THIS:

```
  "ami-atotallyfakeamiid", "ami-0b69ea66ff7391e80"
```

SAVE IT AND FREGOT RELOADS THE FILE AND PRINTS THE UPDATED EVALUATION:

```
Reloaded examples/ami_id/ami_id_debug.rego
{}
```

NOW THAT WE'VE CHANGED THE WHITELIST, THE TERRAFORM PLAN PASSES VALIDATION. HOORAY!