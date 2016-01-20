# haskell-reals-comparison

__A comparison of Haskell exact real number implementations__

_This is work in progress._

The source of the benchmark tasks:  
* [Tasks.PreludeOps](https://github.com/michalkonecny/haskell-reals-comparison/blob/master/src/Tasks/PreludeOps.hs) assuming a Prelude [Floating](https://hackage.haskell.org/package/base-4.8.1.0/docs/Prelude.html#t:Floating) instance
* [Tasks.AERN2Ops](https://github.com/michalkonecny/haskell-reals-comparison/blob/master/src/Tasks/AERN2Ops.hs) assuming an AERN2 [ArrowReal](https://github.com/michalkonecny/aern2/blob/master/aern2-num/src/AERN2/Num/Operations.hs) instance

The benchmark timings are obtained on a Dell Inspiron 15R with 8GB RAM,
Intel(R) Core(TM) i7-3632QM CPU @ 2.20GHz running Ubuntu 14.04.

The benchmarks have been compiled using ghc-7.8.4 with -O2.

Each benchmark has been executed 3 times in a row and the timing has been stable.

| Implementation | Status | Language | logistic0 (n=100) | logistic1 (n=1000)  | logistic2 (n=10000)  |
| -------------- | ------ | ------------ | ---- | --------- | --------- |
| [ireal](https://hackage.haskell.org/package/ireal), IReal, Floating instance | pure Haskell | fairly complete, 2015-10-31 | 0.04 s | 19.8 s | > 10 min |
| [aern2](https://github.com/michalkonecny/aern2), CauchyReal, Floating instance | [haskell-mpfr](https://github.com/comius/haskell-mpfr) | experimental, 2016-01-20 | 0.07 s | 8.7 s | > 10 min |
| [aern2](https://github.com/michalkonecny/aern2), CauchyReal, ArrowReal instance | [haskell-mpfr](https://github.com/comius/haskell-mpfr) | experimental, 2016-01-20 | 0.01 s | 0.06 s | 3.2 s |
| [aern2](https://github.com/michalkonecny/aern2), iRRAM-style MPBall, ArrowReal | [haskell-mpfr](https://github.com/comius/haskell-mpfr) | experimental, 2016-01-20 | 0.01 s | 0.09 s | 5.0 s |
