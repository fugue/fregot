# CHANGELOG

 -  0.14.2 (2022-06-27)
     *  Support `every in`

 -  0.14.1 (2022-06-24)
     *  Build binaries in github actions
     *  Upgrade dependencies

 -  0.14.0 (2022-06-23)
     *  Support `in` and `some in`
     *  Support negative numbers in `format_int`
     *  Support expressions as rule values
     *  Allow comparing things other than numbers
     *  Add ceil, floor, round builtins
     *  Detect some rules that are better computed bottom-up
     *  Improve error message for standalone rule names
     *  Internal: Smarter array union & Delay union on array types
     *  Docs: Update walkthrough to work with current version of Fregot,
        delete duplicative version

 -  0.13.4 (2021-03-03)
     *  Improvements to object unification
     *  Supported nested `==` expressions, e.g. `good := (1 == 1)`.
     *  New builtins:
         -  `object.union`
         -  `regex.find_all_string_submatch_n`
         -  `regex.find_n`

 -  0.13.3 (2021-02-10)
     *  Bump `vector` dependency, GHC version and stackage LTS.

 -  0.13.2 (2021-02-10)
     *  Bump `aeson` and `base64-bytestring` dependencies.
     *  Extend rule dependencies to use suffixes of rule keys, even if they
        are unknown.

 -  0.13.1 (2020-12-10)
     *  Upgrade `lens`, `tasty` and `haskeline` dependencies.
     *  Build a static executable for the Linux binary release.

 -  0.13.0 (2020-11-25)
     *  New builtins:
         -  `abs`
         -  `bits.and`
         -  `bits.lsh`
         -  `bits.negate`
         -  `bits.or`
         -  `bits.rsh`
         -  `bits.xor`
         -  `graph.reachable`
         -  `is_null`
         -  `json.is_valid`
         -  `json.patch`
         -  `numbers.range`
         -  `object.get`
         -  `regex.is_valid builtins`
         -  `regex.match`
         -  `type_name`
         -  `yaml.is_valid`
         -  `yaml.marshal`
         -  `yaml.unmarshal`
     * New features:
         -  Add `--{,no-}strict-builtin-errors` flags
         -  Add an -O flag to explicitly turn on optimizations.
            This will enable the optimizations even when debugging: see #229.
         -  Support indirect references, e.g. `split("foo/bar")[_]`
         -  Add `fregot capabilities` subcommand
         -  Add a verbosity flag to silence the repl
     * Other improvements:
         -  Refactor type checking for builtins to construct a shallow as well
            as a deep embedding of the type, used for type checking and type
            printing respectively
         -  Use the new typing mechanisms to improve the flow typing for:
             *  `array.concat`
             *  `array.slice`
             *  `intersection`
         -  Coerce immediately on specific type tests in Infer.  This improves
            type checking in the presence of `is_null`, `is_array`, ... kinds of
            functions
         -  Better error when failing to unify a function type
         -  Only use colors if we detect that `stderr` / `stdout` supports ANSI
         -  Sort the row context in the REPL to make it deterministic
         -  `min`/`max` should return undefined rather than `null`

 -  0.12.3 (2020-09-02)
     *  Fix a bug in indexing set rules using literal values.
     *  Improve typing for arrays.  We allow more granular, per-index types
        which is especially helpful when using arrays as tuples.

 -  0.12.2 (2020-08-07)
     *  Support using data imports as variables directly

 -  0.12.1 (2020-08-06)
     *  Add `object.remove`
     *  Add `object.filter`
     *  Fix issue in how and when we ground rule indices
     *  Fix issue with `foo` when using `import input.foo`
     *  Sort undefined vars in error message
     *  Internals: Allow unification to restrict types and terms

 -  0.12.0 (2020-07-15)
     *  Add comprehension index optimization.
     *  Add `array.slice` builtin.
     *  Rewrite `==` as just `=`.
     *  Add a `--dump` flag to get specific debug info.
     *  Fix: Some uses `,` not `.` for multiple declarations.
     *  Add support for declaring rules using `:=`.
     *  Improve warning about new packages.
     *  Add `trace` builtin.
     *  Allow prefixing data into packages.
     *  Allow :load-ing JSON and YAML files from the REPL.

 -  0.11.1 (2020-05-28)
     *  Fix zero `with` statements wiping caches.  This was a serious
        performance regression introduced in `v0.9.0`.
     *  Always use lenient decoding for base64 strings.

 -  0.11.0 (2020-05-20)
     *  You can now remove a breakpoint again by calling `:break` with the
        same argument that you used to set it.
     *  Fix bug in reification of packages as trees.
     *  Make indexing type errors produce an empty result rather than throwing
        an error.
     *  Print the row context in addition to the result in the REPL.

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
     *  Type error when using saturated call result

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
