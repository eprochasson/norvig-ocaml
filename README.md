# OCaml Sudoku Solver

Based on the algorithm described (http://norvig.com/sudoku.html)[here].

It works. And is indeed blazing fast.

The ocaml solution has more lines than Norvig's python solution, for the following reasons:

- no list comprehension in ocaml as in python
- I tend to abuse line return to make the code easier to read
- it's pretty much a direct port (the only noticeable change is that the `assign` and `eliminate` function returns
  `unit`, to highlight the fact that they have side effects).
- I suck at ocaml, there should be ways to make the whole thing much better.

## Benchmark

Got the following output from a subset of the problems use in Norvig's page (because the other are not that easily accessible).

```
Done. Solved hard problems (95 problems) in 1.963730s, avg. 0.020671 s/problem (48.377320 Hz)
Done. Solved hardest problems (11 problems) in 0.077885s, avg. 0.007080 s/problem (141.234029 Hz)
```

It's about twice as fast as Norvig's, which tells us nothing since it's not the same machine (I'm not even sure it's the same _decade_ either).

## Licence

MIT
