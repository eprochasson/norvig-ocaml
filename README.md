# OCaml Sudoku Solver

Based on the algorithm described [here](http://norvig.com/sudoku.html).

It works. And is indeed blazing fast.

The OCaml solution is longer than Norvig's Python solution, for the following reasons:

- no list comprehension in OCaml, making a lot of definition quite more fiddly
- I tend to abuse line return to make the code easier to read
- I suck at OCaml, there should be ways to make the whole thing much better.

The first version (see tag `v1.0`) was about half as slow as the original Python code, on the same machine. I spent a
bit of time making it more _OCaml-like_, bringing the following optimizations:

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

As of version 2.1, I get the following output from a subset of the problems use in Norvig's page
(because the other are not that easily accessible), including the "hard1" he generated (that takes 188.79s to solve).

```
Done. Solved hard problems (95 problems) in 0.132424s, avg. 0.001394 s/problem (717.391989 Hz | max: 0.008948s)
Done. Solved hardest problems (11 problems) in 0.004766s, avg. 0.000433 s/problem (2307.905758 Hz | max: 0.000800s)
Done. Solved artificially hard (1 problems) in 6.557443s, avg. 6.557443 s/problem (0.152498 Hz | max: 6.557443s)
```

For comparison, on the same laptop, here are the results obtained by Norvig's script:

```
All tests pass.
Solved 95 of 95 hard puzzles (avg 0.01 secs (96 Hz), max 0.05 secs).
Solved 11 of 11 hardest puzzles (avg 0.00 secs (253 Hz), max 0.01 secs).
Solved 1 of 1 'hard1' puzzles (avg 50.04 secs (0 Hz), max 50.04 secs).
```

The OCaml version appears to be 7 to 10 times faster than the Python implementation.

## Discussion

As described earlier. the possible values for a square are coded in a 9 bit int (actually a native OCaml 31 bit int,
  but I only care about the first 9). Most operation to extract undecided values are therefore simple bitwise
  operations. For example, a square with value `[1;3;4]` is coded as `101100000`. Removing the values `4` is done by
  calculating a bit mask with `2**(4-1) xor 511` (`511` being coded as `111111111`, we get `111011111`), then applying a
  bitwise and with the original value: `101100000 and 111011111 -> 101000000`, effectively eliminating the value `4`
  from the square in as little as 2 cpu cycles (of course, depending of the OCaml compilation). It also has no effect if
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
Done. Solved hard problems (95 problems) in 0.180535s, avg. 0.001900 s/problem (526.214916 Hz | max: 0.012654s)
Done. Solved hardest problems (11 problems) in 0.006492s, avg. 0.000590 s/problem (1694.419332 Hz | max: 0.001093s)
Done. Solved artificially hard (1 problems) in 9.439227s, avg. 9.439227 s/problem (0.105941 Hz | max: 9.439227s)
```

I then tried the `Int` module implementation:

```
let power_2 deg = Int.pow 2 deg
```

and its performance:

```
Done. Solved hard problems (95 problems) in 0.994730s, avg. 0.010471 s/problem (95.503257 Hz | max: 0.068248s)
Done. Solved hardest problems (11 problems) in 0.035304s, avg. 0.003209 s/problem (311.583019 Hz | max: 0.006209s)
Done. Solved artificially hard (1 problems) in 55.075546s, avg. 55.075546 s/problem (0.018157 Hz | max: 55.075546s)
```

This tanks the performance down to the Python version!

Finally, implemented as a bit shift:

```
let power_2 deg = 1 lsl deg (* Shifts 100000000... deg times *)
```

getting the final performance:

```
Done. Solved hard problems (95 problems) in 0.132424s, avg. 0.001394 s/problem (717.391989 Hz | max: 0.008948s)
Done. Solved hardest problems (11 problems) in 0.004766s, avg. 0.000433 s/problem (2307.905758 Hz | max: 0.000800s)
Done. Solved artificially hard (1 problems) in 6.557443s, avg. 6.557443 s/problem (0.152498 Hz | max: 6.557443s)
```

## Licence

MIT
