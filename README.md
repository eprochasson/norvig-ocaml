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

For comparison, on the same laptop, here are the results obtained by Norvig's script:

```
All tests pass.
Solved 95 of 95 hard puzzles (avg 0.01 secs (97 Hz), max 0.05 secs).
Solved 11 of 11 hardest puzzles (avg 0.00 secs (256 Hz), max 0.01 secs).
```

The ocaml version seems to need quite some optimisation.

## Licence

MIT
