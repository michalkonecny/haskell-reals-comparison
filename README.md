# haskell-reals-comparison

A comparison of Haskell exact real number implementations

_This is ongoing work._

_Everyone is welcome to contribute, especially authors of Haskell exact real software._

## Benchmark setup

The benchmark timings are obtained on a
<!-- Dell Inspiron 15R with 16GB RAM,
Intel(R) Core(TM) i7-3632QM CPU @ 2.20GHz running Ubuntu 14.04. -->
<!-- Lenovo T440p with 16GB RAM,
Intel(R) Core(TM) i7-4710MQ CPU @ 2.50GHz running Ubuntu 14.04. -->
HP PC with 16GB RAM,
Intel(R) Core(TM) i7-2600 CPU @ 3.40GHz running Ubuntu 18.04.

The benchmarks have been compiled using ghc-8.4.4.

<!-- Each benchmark has been executed repeatedly until 3 consecutive times the results have fluctuated for less than 5%. -->

## Implementations

| Implementation | Notable dependencies | Status | Reliability | Release date of the version used here |
| ----- | ----- | ----- | ----- | ----- |
| [ireal](https://hackage.haskell.org/package/ireal) | __pure Haskell__ | fairly complete, on Hackage | well tested | 2015-10-31 |
| [aern2-real](https://github.com/michalkonecny/aern2/aern2-real) | [rounded (MPFR)](http://hackage.haskell.org/package/rounded) | fairly complete, on Hackage | well tested | 2019-03-20 |
| [aern2-real-CDAR](https://github.com/michalkonecny/aern2/aern2-real) | [CDAR-mBound](https://github.com/michalkonecny/cdar/tree/mBound) | fairly complete, on Hackage | well tested | 2019-03-20 |
| [CDAR](https://github.com/jensblanck/cdar) | __pure Haskell__ | on GitHub | well documented, based on [theory](http://cs.swan.ac.uk/%7Ecsjens/pdf/centred.pdf) | 2018-11-23 |
| [CDAR-mBound](https://github.com/michalkonecny/cdar/tree/mBound) | __pure Haskell__ | on GitHub | CDAR + with a bit-size bound for midpoint | 2019-03-20 |

The source of the benchmark tasks: 
* [Tasks.PreludeOps](https://github.com/michalkonecny/haskell-reals-comparison/blob/master/src/Tasks/PreludeOps.hs) assuming a Prelude [Floating](https://hackage.haskell.org/package/base/docs/Prelude.html#t:Floating) instance
* [Tasks.MixedTypesNumOps](https://github.com/michalkonecny/haskell-reals-comparison/blob/master/src/Tasks/MixedTypesNumOps.hs) assuming instances of relevant classes in [MixedTypesNumPrelude](https://hackage.haskell.org/package/mixed-types-num/docs/MixedTypesNumPrelude.html)

For some implementations, we used more than one evaluation strategy.  In particular, for AERN2 and ireal, we use the following strategies:

* __Lazy Cauchy style__: Composing operations over real numbers using a lazy composition of (fast) Cauchy sequences, ie querying the number with certain accuracy 2^(-n) given by a natural number n. 

* __iRRAM style__: Computing using interval arithmetic at a fixed precision for the centers of the intervals.  When the radius grows too large, stop and restart the computation with a higher precision.  Repeat until the result is sufficiently accurate.  This approach is usually a bit less convenient for programmers than lazy Cauchy real numbers since one usually needs to explicitly invoke the iterative process at the top level.

CDAR uses an iRRAM style evaluation strategy internally and it cannot be easily switched to using a lazy Cauchy style.
Nevertheless, in CDAR the iRRAM style evaluation is entirely hidden from the programmer, making it as convenient as lazy Cauchy style, 
ie having a simple to use "real number" type.

Note that "precision" in CDAR corresponds to absolute errors whereas "precision" in iRRAM, AERN2 and MPFR
corresponds to relative errors.  CDAR-mBound is a version with an additional floating-point precision control.

## Benchmark results

Results of the "logistic" benchmark, running n iterations of the logistic map with c=3.82 and x0=0.125.

| Implementation | strategy | n=100 | n=316 | n=1000 | n=3160 | n=10000  | n=31600 | n=100000 |
| -------- | ------ | ---- | ---- | ---- | ---- | ---- | ---- | ---- |
| aern2 | Cauchy seq. | 0.02 s | 0.07 s / 10&nbsp;MB | 0.23 s  / 15&nbsp;MB  | 0.86 s / 37&nbsp;MB | 3.9 s / 243&nbsp;MB |  23 s / 900&nbsp;MB | 193 s / 7.8&nbsp;GB |
| aern2-CDAR | Cauchy seq. | 0.03 s | 0.12 s / 9&nbsp;MB | 0.42 s  / 14&nbsp;MB  | 1.68 s / 39&nbsp;MB | 10.3 s / 146&nbsp;MB |  111 s / 750&nbsp;MB | 1604 s / 5.2&nbsp;GB |
| aern2 | iRRAM-style | 0.01 s | 0.02 s / 7&nbsp;MB  | 0.05 s  / 7&nbsp;MB  | 0.44 s  / 7&nbsp;MB  | 2.3 s /  7&nbsp;MB  | 43 s /  11&nbsp;MB | 903 s / 15&nbsp;MB |
| aern2-CDAR | iRRAM-style | 0.01 s | 0.03 s / 7&nbsp;MB  | 0.10 s  / 7&nbsp;MB  | 0.63 s  / 7&nbsp;MB  | 2.7 s /  8&nbsp;MB  | 46 s /  10&nbsp;MB | 903 s / 15&nbsp;MB |
| CDAR | iRRAM-style | 0.01 s | 0.01 s / 7&nbsp;MB | 0.02 s  / 9&nbsp;MB  | 0.1 s / 13&nbsp;MB | 1.24 s / 45&nbsp;MB |  17 s / 95&nbsp;MB | 272 s / 195&nbsp;MB |
| CDAR-mBound | iRRAM-style | 0.01 s | 0.01 s / 7&nbsp;MB | 0.02 s  / 9&nbsp;MB  | 0.2 s / 14&nbsp;MB | 1.4 s / 43&nbsp;MB |  18 s / 97&nbsp;MB | 283 s / 220&nbsp;MB |
| ireal | iRRAM-style | 0.01 s | 0.01 s / 6&nbsp;MB  | 0.03 s  / 6&nbsp;MB  | 0.12 s  / 6&nbsp;MB  | 2.4 s / 7&nbsp;MB   | 53 s / 8&nbsp;MB   | 375 s / 9&nbsp;MB   |

The charts show a few more data points:

| Execution time | Max RAM use |
| :---: | :---: |
| <img src="benchmarks/charts/logistic-time.png?raw=true" width="400"> | <img src="benchmarks/charts/logistic-space.png?raw=true" width="400"> |

## TODO
* include package [exact-real](https://hackage.haskell.org/package/exact-real)
* include package [haskell-fast-reals](https://github.com/comius/haskell-fast-reals)
* add more benchmarks,eg FFT, inverting a Hilbert matrix
* (ongoing) regularly update for newer versions
