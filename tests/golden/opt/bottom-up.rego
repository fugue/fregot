package bottom_up

primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]

double_primes[ret] {
  # Should be bottom up, `ret` only assigned.
  primes[p]
  ret := p * 2
}

small_primes[ret] {
  # Should not be bottom up because of the restriction on `ret`.
  ret := primes[_]
  ret < 10
}
