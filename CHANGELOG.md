# CHANGELOG

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
     *  Allow YAML input documents

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
