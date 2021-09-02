# Dedekind cuts

We can define a real number `x` by taking two partitions, `(A,B)`, such that in
`A` are all numbers that are *less than* `x`, and in `B` are all numbers that
are *bigger than* `x`.

We state that:
  1) `A` is always inhabited,
  2) `B` is always inhabited,
  3) if `a` is in `A`, and `c < a`, then `c` is in `A`,
  4) if `b` is in `B`, and `c > b`, then `c` is in `B`,
  5) if `b` is not in `B`, and `a < b`, then `a` is in `A`,
  6) if `a` is not in `A`, and `b > a`, then `b` is in `B`,
  7) for every `a` in `A` there is `b > a` such that `b` is in `A`,
  8) for every `b` in `B` there is `a < b` such that `a` is in `B`.

A pair that satisfies these properties is then called a *Dedekind cut*.
