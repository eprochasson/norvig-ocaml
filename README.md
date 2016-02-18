# OCaml Sudoku Solver

Based on the algorithm described (http://norvig.com/sudoku.html)[here].

It works. And is indeed blazing fast.

The ocaml solution has more lines than Norvig's python solution, for the following reasons:
- no list comprehension in ocaml as in python
- I tend to abuse line return to make the code easier to read
- it's pretty much a direct port (the only noticeable change is that the `assign` and `eliminate` function returns
  `unit`, to highlight the fact that they have side effects).
- I suck at ocaml, there should be ways to make the whole thing much better.

## Benchmark: todo

Licence: MIT
