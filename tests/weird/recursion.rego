# Is recursion allowed?  Of course not.
#
# In this package (`recursion`), we have a rule called `foo`.  Within that rule,
# we cannot use `data.recursion.foo`, or the compiler will yell at us.
#
# However, `data` accesses can be dynamic and we try to sufficiently hide what
# we're doing, there is no way to tell that recursion will happen except by
# evaluating the policy!
#
# The sane solution here would be to add a recursion check to the evaluation as
# well.

package recursion

pkg = "recursion"

foo[x] {
  data[pkg]["foo"][x]
}

test_foo {
  foo[1]
}
