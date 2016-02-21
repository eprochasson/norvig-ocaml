# OCaml Sudoku Solver

Based on the algorithm described (http://norvig.com/sudoku.html)[here].

It works. And is indeed blazing fast.

The ocaml solution is longer than Norvig's python solution, for the following reasons:

- no list comprehension in ocaml, making a lot of definition quite more fiddly
- I tend to abuse line return to make the code easier to read
- I suck at ocaml, there should be ways to make the whole thing much better.

The first version (see tag `v1.0`) was about half as slow as the original python code, on the same machine. I spent a
bit of time making it more _ocaml-like_, bringing the following optimizations:

- The grid is an array of `int` of size 81, one cell per square, instead of a hash table. a 9 bit int can encode all
the possible values for a square (I used standard int, I didn't bother optimizing further using 16 bit int or such).
All the operation to assign a value to a square or check the undecided values of a given square are therefore just
bitwise operations, which are blazing fast.
My original version was an array of list of int, each cell containing a list of undecided values (a list of size one
  indicating a decided square).
- Instead a hash map to quickly access all the units/peers of a square, I mapped those in 0 indexed arrays. It's
therefore very easy to get all the peers or units for a given square, since its index on the grid and its index in the
unit and peers map are identical. Although hash map should be in `O(1)`, we're clearly not trying to change the big-O
complexity of the program here, but rather shave ms here and there, using a constant time access Array helps.
- Instead of using a coordinate system (e.g.: `(a, 1)`), I use plain integer, as index of the grid. It makes the code a
 little harder to read, but a lot easier to write.

## Benchmark

As of version 2.1, Got the following output from a subset of the problems use in Norvig's page
(because the other are not that easily accessible).

```
Done. Solved hard problems (95 problems) in 0.132834s, avg. 0.001398 s/problem (715.177287 Hz)
Done. Solved hardest problems (11 problems) in 0.004731s, avg. 0.000430 s/problem (2325.119387 Hz)
```

For comparison, on the same laptop, here are the results obtained by Norvig's script:

```
All tests pass.
Solved 95 of 95 hard puzzles (avg 0.01 secs (96 Hz), max 0.05 secs).
Solved 11 of 11 hardest puzzles (avg 0.00 secs (247 Hz), max 0.01 secs).
```

The final ocaml version is 7 to 10 times faster than the python implementation.

## Discussion

As described earlier. the possible values for a square are coded in a 9 bit int (actually a native ocaml 31 bit int,
  but I only care about the first 9). Most operation to extract undecided values are therefore simple bitwise
  operations. For example, a square with value `[1;3;4]` is coded as `101100000`. Removing the values `4` is done by
  calculating a bit mask with `2**(4-1) xor 511` (`511` being coded as `111111111`, we get `111011111`), then applying a
  bitwise and with the original value: `101100000 and 111011111 -> 101000000`, effectively eliminating the value `4`
  from the square in as little as 2 cpu cycles (of course, depending of the ocaml compilation). It also has no effect if
  `4` is not a value to be removed.

I therefore needed a function to elevate value to power of 2. Here is the first version I used:

```
let power_2 deg = (* Original version *)
 let rec _sq2 acc = function
 | 0 -> acc
 | x -> _sq2 (2*acc) (x-1)
 in _sq2 1 deg
```

and its performance (everything else being equal):

```
Done. Solved hard problems (95 problems) in 0.188412s, avg. 0.001983 s/problem (504.214300 Hz)
Done. Solved hardest problems (11 problems) in 0.006787s, avg. 0.000617 s/problem (1620.730811 Hz)
```

I then tried the `Int` module implementation:

```
let power_2 deg = Int.pow 2 deg
```

and its performance:

```
Done. Solved hard problems (95 problems) in 0.985114s, avg. 0.010370 s/problem (96.435530 Hz)
Done. Solved hardest problems (11 problems) in 0.035017s, avg. 0.003183 s/problem (314.133014 Hz)
```

This tanks the performance down to the python version!

Finally, implemented as a bit shift:

```
let power_2 deg = 1 lsl deg (* Shifts 100000000... deg times *)
```

getting the final performance:

```
Done. Solved hard problems (95 problems) in 0.132834s, avg. 0.001398 s/problem (715.177287 Hz)
Done. Solved hardest problems (11 problems) in 0.004731s, avg. 0.000430 s/problem (2325.119387 Hz)
```

## Licence

MIT
