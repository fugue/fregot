# CHANGELOG

 -  0.10.1 (2020-05-07)
     *  Upgrade to GHC 8.8 and stack LTS-15.6.
     *  Loosen restrictions on the LHS of `:=`.  This allows you to assign
        to arrays, e.g.:

            [_, num, _] := split("routes.1234.cidr_block", ".")

 -  0.10.0 (2020-05-04)
     *  Add `json.marshal` builtin function.
     *  Fix isssue where `{}` was treated as an empty set, not an empty object.
     *  Add `base64` builtin functions:

         -  `base64.encode`
         -  `base64.decode`
         -  `base64url.encode`
         -  `base64url.decode`
     *  Add `io.jwt` builtin functions:

         -  `io.jwt.encode_sign`
         -  `io.jwt.decode`
         -  `io.jwt.decode_verify`

        These use an external library and will not work for all key types.  We
        can resolve that as demand for other key types comes up.

 -  0.9.1 (2020-04-17)
     *  Better error for foo.bar() if foo is not imported
     *  Disallow :reload while debugging
     *  Type error when using satured call result

        You can use functions that take arguments, such as `array.concat` in two
        ways:

        1.  `array.concat([1], [2], out)`
        2.  `out = array.concat([1], [2])` (alternatively using `:=`)

        This makes you wonder what the _return type_ of
        `array.concat([1], [2], out)` is.  We used to have a boolean here, that
        returned whether or not `out` unified with the result.  However, that
        allows us to write code as:

            x = array.concat([1], [2], out)

        Which is almost always an arity mistake on the programmer's side.  This
        release changes the return type of saturated calls to the _void_ type,
        so it can't be assigned or used.

 -  0.9.0 (2020-03-19)
     *  Support `with data` statements in addition to `with input` statements.
     *  Fix issue when using REPL reloading and the `--input` argument.
     *  Fix a crashing when indexing the `null` value with a key.
     *  Fix scoping of `with` statements.  `with` statements used to (wrongly)
        affect the input for the entirety of the query, whereas they should
        really only affect the current statement.

 -  0.8.0 (2020-02-27)
     *  Small performance tweaks.
     *  Add `copyHandle` to interpreter module.
     *  Replace problematic AST nodes by errors.  This drastically reduces the
        amount of errors you see; hopefully leaving only the root causes.

 -  0.7.6 (2020-02-05)
     *  Add `regex.split` builtin function.

 -  0.7.5 (2020-02-04)
     *  Add `is_number` builtin function.
     *  Add more trim builtins functions: `trim_left`, `trim_prefix`,
        `trim_right`, `trim_suffix`, `trim_space`.
     *  Add `--no-history-file` flag to `fregot repl`.
     *  Make the return type of `==` a boolean.
     *  Fix issue with return type of `walk()`.

 -  0.7.4 (2020-01-20)
     *  Add `walk` builtin function.
     *  Allow builtin functions to stream values rather than just returning a
        single one.

 -  0.7.3 (2020-01-13)
     *  Fix typo in "Unknown type" error message.
     *  Fix issue where referencing into `null` would crash.
     *  Fix issue where refactoring into an array using a key would crash.

 -  0.7.2 (2019-12-30)
     *  Ignore partial rules when converting packages to objects.
     *  Fix issue where we would index into packages rather than rules;
        sometimes resulting in empty sets for collection references that had at
        least some dynamic part in them.

 -  0.7.1 (2019-12-25)
     *  Extend `%v` formatting in `sprintf` to work for objects, lists and sets.

 -  0.7.0 (2019-12-19)
     *  Change the return type of set() to set{unknown}.
     *  Add `--input` option to `fregot repl`.
     *  Add support for loading JSON/YAML files as data.
     *  Use a tree datastructure to store rules, fix Value.

        This is a huge refactoring that changes the following things:

         -  We now store rules in a tree structure (see `Fregot.Tree`) rather
            than having a collection of packages with rules.
         -  The evaluation distinguishes between `Value` (a grounded value) and
            `Mu` (a *M*aybe *u*ngrounded value), which allows us to reify
            packages.
         -  References into packages are extended, allowing us to e.g. iterate
            over all packages under a prefix.
         -  Improved dependency tracking for dynamic references to rules.

 -  0.6.0 (2019-12-14)
     *  Add support for raw strings.

 -  0.5.0 (2019-12-09)
     *  Add a type checking phase using flow analysis.
     *  Add the `:type` command to the REPL.

 -  0.4.4 (2019-11-18)
     *  Minor README improvements.
     *  Allow importing `input.` paths.
     *  Improve error when `package` declaration at the start of a file is
        missing.

 -  0.4.3 (2019-11-16)
     *  Add `is_set` and `is_boolean` builtin functions.

 -  0.4.2 (2019-11-14)
     *  Fixes an issue where depth-first iteration over collection rules would
        enumerate certain elements more than once.
     *  Improve caching by also keeping partially enumerated collections around
        and when a regular "exists" query is used, we will now visit the cached
        elements of the collection first to check for hits.
     *  Fix calculation of names used in rules.  This could possibly cause
        issues where the dependencies of a rule were calculated incorrectly,
        causing fregot to throw a renamer error.

 -  0.4.1 (2019-11-06)
     *  Allow YAML input documents.

 -  0.4.0 (2019-11-01)
     *  Re-license to Apache-2.0
     *  The `fregot repl` and `fregot eval` now take queries, not expressions
     *  Fix issues with cache during debugging
     *  Make `:reload` reload all files rather than just the file that was last
        loaded
     *  Add a `--watch` flag to `fregot repl` to watch file changes
     *  Add a `:watch` metacommand in the REPL to watch expressions or other
        metacommands
     *  Allow `:open pkg` as well as `:open data.pkg`
     *  Allow `:break data.pkg.rule` as well as `:break pkg.rule`

 -  0.3.0 (2019-10-11)
     *  Add support for `some` keyword
