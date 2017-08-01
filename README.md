# haskell-reals-comparison

__A comparison of Haskell exact real number implementations__

_This is ongoing work._

_Everyone is welcome to contribute, especially authors of Haskell exact real software._

### Benchmark setup

The benchmark timings are obtained on a
<!-- Dell Inspiron 15R with 16GB RAM,
Intel(R) Core(TM) i7-3632QM CPU @ 2.20GHz running Ubuntu 14.04. -->
Lenovo T440p with 16GB RAM,
Intel(R) Core(TM) i7-4710MQ CPU @ 2.50GHz running Ubuntu 14.04.

The benchmarks have been compiled using ghc-7.10.3 with -O2.

<!-- Each benchmark has been executed repeatedly until 3 consecutive times the results have fluctuated for less than 5%. -->

### Implementations

| Implementation | Notable dependencies | Status | Reliability | Release date of the version used here |
| ----- | ----- | ----- | ----- | ----- |
| [ireal](https://hackage.haskell.org/package/ireal) | __pure Haskell__ | fairly complete, on Hackage | well tested | 2015-10-31 |
| [aern2-real](https://github.com/michalkonecny/aern2/aern2-real) | [hmpfr](https://hackage.haskell.org/package/hmpfr) | fairly complete, on GitHub | well tested | 2017-08-01 |

The source of the benchmark tasks:  
* [Tasks.PreludeOps](https://github.com/michalkonecny/haskell-reals-comparison/blob/master/src/Tasks/PreludeOps.hs) assuming a Prelude [Floating](https://hackage.haskell.org/package/base/docs/Prelude.html#t:Floating) instance
* [Tasks.MixedTypesNumOps](https://github.com/michalkonecny/haskell-reals-comparison/blob/master/src/Tasks/MixedTypesNumOps.hs) assuming instances of relevant classes in [MixedTypesNumPrelude](https://hackage.haskell.org/package/mixed-types-num/docs/MixedTypesNumPrelude.html)

For each benchmark and for each implementation, we used two different evaluation strategies:

* __Cauchy-real style__: Composing operations over real numbers using a lazy composition of Cauchy sequences, ie querying the number with certain accuracy 2^(-n) given by a natural number n.

* __iRRAM style__: Computing using interval arithmetic at a fixed precision for the centers of the intervals.  When the radius grows too large, stop and restart the computation with a higher precision.  Repeat

### Benchmark results

<!-- TODO: Update these measurements! -->
<!-- TODO: add iRRAM-style ireal -->

Results of the "logistic" benchmark, running n iterations of the logistic map with c=3.82 and x0=0.125.

| Implementation | strategy | n=100 | n=320 | n=1000 | n=3200 | n=10000  | n=32000 | n=100000 |
| -------- | ------ | ---- | ---- | ---- | ---- | ---- | ---- | ---- |
| ireal | Cauchy seq. | 0.02 s | 0.53 s / 6 MB  | 14.84 s / 21 MB | 689 s / 173 MB | | | |
| ireal | iRRAM-style | 0.62 s | 1.71 s / 12 MB | 4.22 s  / 13 MB | 17 s  / 18 MB  | 62 s / 19 MB   | 441 s / 25 MB | 2611 s / 41 MB |
| aern2 | Cauchy seq. | 0.02 s | 0.07 s / 14 MB | 0.26 s  / 30 MB | 1.17 s / 93 MB | 7.6 s / 278 MB | | |
| aern2 | iRRAM-style | 0.02 s | 0.05 s / 8 MB  | 0.14 s  / 9 MB  | 1.0 s  / 16 MB | 6.9 s / 16 MB  | 181 s / 33 MB | 1875 s / 95 MB |

### TODO
* include package [exact-real](https://hackage.haskell.org/package/exact-real)
* include package [haskell-fast-reals](https://github.com/comius/haskell-fast-reals)
* add more benchmarks,eg FFT
* (ongoing) regularly update for newer versions
