# This checks if we have proper set deduplication in the evaluation.
#
# This is not a problem when we evaluate the below `numbers`; since that simply
# gives us a hashset.
#
# But when we iterate over it and produce the numbers in the set on demand, we
# may still run into duplicates.  For that reason, we test this using a repl
# query which causes iteration over the `numbers` set.
package set

primes = {
    2, 3, 5, 7, 11
}

fibonacci = {
    0, 1, 1, 2, 3, 5, 8, 13
}

numbers[n] {
    primes[n]
} {
    fibonacci[n]
}
