fregot
======

`fregot` (**F**ugue **Rego** **T**oolkit) is a set of tools for working with the
[Rego] policy language.

It can be seen as an alternative REPL to OPA's built-in interpreter.  The goals
is different though -- `fregot` aims to provide:

 -  Just a Rego language implementation rather than the full agent
 -  Useful tools to debug Rego queries and modules
 -  Great error messages
 -  Ease of extending and experimenting with different language features

In the future, we also hope to improve the static analyzer to prevent many kinds
of bugs.

Table of Contents
-----------------

-   [Table of Contents](#table-of-contents)
-   [Installation](#installation)
    -   [Pre-built packages](#pre-built-packages)
    -   [From source](#from-source)
-   [Running](#running)
-   [REPL](#repl)
    -   [The open package](#the-open-package)
    -   [The input document](#the-input-document)
    -   [Debugging](#debugging)

Installation
------------

### Pre-built packages

TODO: We need to set up CircleCI to built Linux and Mac OS X binaries.

### From source

Installation through source is done using standard Haskell tooling -- [Cabal]
and [stack] should both work fine.

#### Using stack

1.  Install [stack] for your platform.
2.  Clone this repository and `cd` into it.
3.  Run `stack install`.
4.  Make sure `$HOME/.local/bin` is in your `$PATH`.

Running
-------

    fregot SUBCOMMAND

`fregot` understands a number of subcommands:

 -  `fregot repl [PATHS]`: Start a REPL.  See [working with the repl][#repl].

 -  `fregot test [PATHS]`: Run tests.  `fregot` will find Rego recursively look
    for Rego files in the given paths and run any rule starting with `test_`.

 -  `fregot bundle [PATHS]`: Compile a number of Rego files into a single bundle
    that can be loaded faster than individual files.  Experimental.

 -  `fregot eval [--input PATH] EXPRESSION [PATHS]`: Evaluate a Rego expression.

    Use `--input` to use a JSON file as `input` to the policy.

REPL
----

The REPL is currently the most important part of `fregot`.  After loading the
files passed on the command line, you end up on an interactive prompt.

There are three ways to interact with the REPL:

1.  Entering a rule adds the rule to the currently open package, e.g.:

        repl% numbers = {4, 8, 15, 16, 23, 42}
        Rule numbers added

2.  Entering a query evaluates that query, e.g.:

        repl% numbers[n]; n % 2 == 0; n
        = 4
        = 16
        = 8
        = 42

3.  There are number of special commands that start with `:`.  Entering `:help`
    shows you the full list of commands.

        repl% :quit

### The open package

The REPL has the concept of an open package; indicated by the prompt.  Initially
this is `repl`; but you can change this using `:open`.  For example, we can add
a rule to the package `foo` and change back to `repl`:

    repl% :open foo
    foo% a = 1
    Rule a added
    foo% :open repl
    repl% data.foo.a
    = 1

A typical workflow is to have an editor open as well as a `fregot repl`.  You
can then load the file using `:load`, which automatically opens the package:

    repl% :load policy.rego
    Loading policy.rego...
    Loaded package policy
    policy%

Once you make changes to the file, just reload it using `:reload` and test your
changes using `:test`:

    policy% :reload
    Loading test.rego...
    Loaded package policy
    policy% :test
    passed: 1, failed: 0, errored: 0

### The input document

While in the REPL, you can directly change the input document by using the
`:input` command, e.g. if you want to load the input in `example.json`:

    repl% :input example.json
    repl% input
    = {"user": "alice"}

### Debugging

You can start debugging by setting a breakpoint; and then evaluating something.

To set a breakpoint, use the `:break` command.  You can use the `:break` command
with either names, or a position in a file (line number).  For example:

    :break foo            # `foo` in the current package
    :break repl.foo       # `foo` in the package repl
    :break data.repl.foo  # Same as above
    :break foo.rego:9     # Line 9 of `foo.rego`

Once at least one breakpoint is set, you can use `:break` without arguments to
display the list.

Next, evaluate an expression that activates the breakpoint.  If the breakpoint
is set on `foo`, we can just evaluate that:

    %repl foo

Once the breakpoint is activated, you end up in a _debugging context_.  From
here, you can do a number of things:

 -  Enter a query to evaluate in the _current context_: meaning that you can
    print and evaluate local variables that are in scope.
 -  Use `:continue` to continue to the next breakpoint.
 -  Use `:step` and `:next` to step into and over the next query, respectively.
 -  Use `:rewind` to go _back_ to the last step.
 -  Use `:where` to see your current location.

[Cabal]: https://www.haskell.org/cabal/
[Rego]: https://www.openpolicyagent.org/docs/latest/policy-language/
[stack]: https://docs.haskellstack.org/en/stable/README/
