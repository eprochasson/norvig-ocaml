# OCaml Sudoku Solver

Based on the algorithm described [here](http://norvig.com/sudoku.html).

It works. And is indeed blazing fast.

The ocaml solution is longer than Norvig's python solution, for the following reasons:

- no list comprehension in ocaml, making a lot of definition quite more fiddly
- I tend to abuse line return to make the code easier to read
- I suck at ocaml, there should be ways to make the whole thing much better.

The first version (see tag `v1.0`) was about half as slow as the original python code, on the same machine. I spent a
bit of time making it more _ocaml-like_, bringing the following optimizations.

- The grid is an array of `int` of size 81, one cell per square, instead of a hash table. a 9 bit int can encode all
the possible values for a square (I used standard int, I didn't bother optimizing further using 16 bit int or such).
All the operation to assign a value to a square or check the undecided values of a given square are therefore just
bitwise operations, which are blazing fast and also cool.
On that note, this didn't bring the time down at all (no significant change in speed). But it was worse a shot. The
original version was an array of list of int, each cell containing a list of undecided values (a list of size one
  indicating a decided square).
- Instead a hash map to quickly access all the units/peers of a square, I mapped those in 0 indexed arrays. It's
therefore very easy to get all the peers or units for a given square, since its index on the grid and its index in the
unit and peers map are identical.
- Instead of using a coordinate system (e.g.: `(a, 1)`), I use plain integer, as index of the grid array. This doesn't
make much difference. It makes the code a little bit harder to read, but a lot more easier to write.

## Benchmark

Got the following output from a subset of the problems use in Norvig's page
(because the other are not that easily accessible), using a natively compiled code.

```
Done. Solved hard problems (95 problems) in 0.188034s, avg. 0.001979 s/problem (505.227623 Hz)
Done. Solved hardest problems (11 problems) in 0.006785s, avg. 0.000617 s/problem (1621.243376 Hz)
```

For comparison, on the same laptop, here are the results obtained by Norvig's script:

```
All tests pass.
Solved 95 of 95 hard puzzles (avg 0.01 secs (96 Hz), max 0.05 secs).
Solved 11 of 11 hardest puzzles (avg 0.00 secs (247 Hz), max 0.01 secs).
```

The ocaml version is 5 to 6 times faster than the python implementation.

FWIW, results for bytecode-compiled code:

```
Done. Solved hard problems (95 problems) in 2.373720s, avg. 0.024987 s/problem (40.021571 Hz)
Done. Solved hardest problems (11 problems) in 0.086108s, avg. 0.007828 s/problem (127.746596 Hz)
```

## Licence

MIT
