# haskell-reals-comparison

A comparison of Haskell exact real number implementations

_This is ongoing work._

_Everyone is welcome to contribute, especially authors of Haskell exact real software._

<!-- Each benchmark has been executed repeatedly until 3 consecutive times the results have fluctuated for less than 5%. -->

## Implementations

| Implementation | Notable dependencies | Status | Reliability | Release date of the version used here |
| ----- | ----- | ----- | ----- | ----- |
| [ireal](https://hackage.haskell.org/package/ireal) | __pure Haskell__ | fairly complete, on Hackage | well tested | (currently not compiling) |
| [aern2-real](https://github.com/michalkonecny/aern2/aern2-real) | [rounded (MPFR)](http://hackage.haskell.org/package/rounded) | fairly complete, on Hackage | well tested | 2022-07-13 |
| [CDAR](https://github.com/jensblanck/cdar) | __pure Haskell__ | on GitHub | well documented, based on [theory](http://cs.swan.ac.uk/%7Ecsjens/pdf/centred.pdf) | 2018-11-23 |
| [CDAR-mBound](https://github.com/michalkonecny/cdar/tree/mBound) | __pure Haskell__ | on GitHub | CDAR + with a bound for mantissa size | 2019-03-20 |

Note that "precision" in CDAR corresponds to absolute error bounds whereas "precision" in iRRAM, AERN2 and MPFR
corresponds to relative error bounds (ie the mantissa size).  CDAR-mBound is a version amended with a mantissa size control.

## Benchmark tasks source code

* [Tasks.PreludeOps](https://github.com/michalkonecny/haskell-reals-comparison/blob/master/src/Tasks/PreludeOps.hs) assuming a Prelude [Floating](https://hackage.haskell.org/package/base/docs/Prelude.html#t:Floating) instance
* [Tasks.MixedTypesNumOps](https://github.com/michalkonecny/haskell-reals-comparison/blob/master/src/Tasks/MixedTypesNumOps.hs) assuming instances of relevant classes in [MixedTypesNumPrelude](https://hackage.haskell.org/package/mixed-types-num/docs/MixedTypesNumPrelude.html)

<!-- For some implementations, we used more than one evaluation strategy.  In particular, for AERN2 and ireal, we use the following strategies: -->

For AERN2 we use two different evaluation strategies:

* __Lazy Cauchy style__: Composing operations over real numbers using a lazy composition of (fast) Cauchy sequences, ie querying the number with certain accuracy 2^(-n) given by a natural number n. 

* __Iterative style__: Computing using interval arithmetic at a fixed precision for the centers of the intervals.  When the radius grows too large, stop and restart the computation with a higher precision.  Repeat until the result is sufficiently accurate.  This approach is usually a bit less convenient for programmers than lazy Cauchy real numbers since one usually needs to explicitly invoke the iterative process at the top level.

<!-- CDAR uses an iterative style evaluation strategy internally and it cannot be easily switched to using a lazy Cauchy style.
Nevertheless, in CDAR the iRRAM style evaluation is entirely hidden from the programmer, making it as convenient as lazy Cauchy style, 
ie having a simple to use "real number" type. -->

## Benchmark results

[click here](http://michalkonecny.github.io/haskell-reals-comparison/results.html)

## TODO
* include package [exact-real](https://hackage.haskell.org/package/exact-real)
* include package [haskell-fast-reals](https://github.com/comius/haskell-fast-reals)
* add more benchmarks,eg FFT, inverting a Hilbert matrix
* (ongoing) regularly update for newer versions
