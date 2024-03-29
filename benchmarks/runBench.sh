#!/bin/bash

results_file=$1
if [ "$results_file" == "" ]; then echo "usage: $0 <results JS file name>"; exit 1; fi

benchmain=haskell-reals-comparison
#benchmain=haskell-reals-comparison-mpfr-origcdar
# benchmain=haskell-reals-comparison-cdar-mbound

reuselogs="true"

# put headers in the results csv file if it is new:
if [ ! -f $results_file ]
    then echo "const allData = [" > $results_file
    if [ $? != 0 ]; then exit 1; fi
fi

function runForBenchParamss
{
  for benchParams in $bparamss
  do
    runOne
  done
}

function runOne
# parameters:
#  $dir where to put individual logs
#  $bench
#  $benchParams
#  $method
#  $params
{
    runlog="$dir/run-$bench-$benchParams-$method-${params// /_}.log"
    echo -n /usr/bin/time -v $benchmain $bench $benchParams $method $params
    if [ ! -f $runlog ] || grep -q "terminated" $runlog
        then
            echo " (running and logging in $runlog)"
            /usr/bin/time -v $benchmain $bench $benchParams $method $params >& $runlog
            if [ $? != 0 ]; then rm $runlog; exit 1; fi
            getDataFromRunlog
        else
          if [ "$reuselogs" = "" ]
            then
              echo " (skipping due to existing log $runlog)"
            else
              echo " (reusing existing log $runlog)"
              getDataFromRunlog
          fi
    fi
}

function getDataFromRunlog
{
  utime=`grep "User time (seconds)" $runlog | sed 's/^.*: //'`
  stime=`grep "System time (seconds)" $runlog | sed 's/^.*: //'`
  mem=`grep "Maximum resident set size (kbytes)" $runlog | sed 's/^.*: //'`
  exact=`grep -i "accuracy: Exact" $runlog | sed 's/accuracy: Exact/exact/'`
  bits=`grep -i "accuracy: bits " $runlog | sed 's/accuracy: [bB]its //'`
  now=`date`
  echo "{time: \"$now\", bench: \"$bench\", param: $benchParams, method: \"$method\", utime: ${utime/0.00/0.01}, stime: ${stime/0.00/0.01}, mem: $mem }," >> $results_file
}

# Time,Bench,BenchParams,Method,AccuracyTarget(bits),Accuracy(bits),UTime(s),STime(s),Mem(kB)

function runForAllMethods
{
  # if [ "$method_ireal_CR_bparamss" != "" ]; then
  #   method="ireal_CR"; bparamss="$method_ireal_CR_bparamss" method_ireal_CR_bparamss=""
  #   runForBenchParamss
  # fi
  # if [ "$method_ireal_MP_bparamss" != "" ]; then
  #   method="ireal_MP"; bparamss="$method_ireal_MP_bparamss" method_ireal_MP_bparamss=""
  #   runForBenchParamss
  # fi
  # if [ "$method_ireal_MP1_bparamss" != "" ]; then
  #   method="ireal_MP1"; bparamss="$method_ireal_MP1_bparamss" method_ireal_MP1_bparamss=""
  #   runForBenchParamss
  # fi
  # if [ "$method_ireal_MP2_bparamss" != "" ]; then
  #   method="ireal_MP2"; bparamss="$method_ireal_MP2_bparamss" method_ireal_MP2_bparamss=""
  #   runForBenchParamss
  # fi
  if [ "$method_cdar_bparamss" != "" ]; then
    method="cdar_mBound"; bparamss="$method_cdar_bparamss" method_cdar_bparamss=""
    runForBenchParamss
  fi
  if [ "$method_aern2_CR_bparamss" != "" ]; then
    method="aern2_CR"; bparamss="$method_aern2_CR_bparamss" method_aern2_CR_bparamss=""
    runForBenchParamss
  fi
  if [ "$method_aern2_MP_bparamss" != "" ]; then
    method="aern2_MP"; bparamss="$method_aern2_MP_bparamss" method_aern2_MP_bparamss=""
    runForBenchParamss
  fi
  if [ "$method_aern2_MP1_bparamss" != "" ]; then
    method="aern2_MP1"; bparamss="$method_aern2_MP1_bparamss" method_aern2_MP1_bparamss=""
    runForBenchParamss
  fi
  if [ "$method_aern2_MP2_bparamss" != "" ]; then
    method="aern2_MP2"; bparamss="$method_aern2_MP2_bparamss" method_aern2_MP2_bparamss=""
    runForBenchParamss
  fi
}

#################
### logistic
#################

function logisticAllMethodsFine
{
  steps100="100 133 177 237 316 421 562 749"
  steps1000="1000 1330 1770 2370 3160 4210 5620 7490"
  steps10000="10000 13300 17700 23700 31600 42100 56200 74900"

  method_cdar_bparamss="$steps100 $steps1000 $steps10000 100000";
  method_aern2_CR_bparamss="$steps100 $steps1000 $steps10000 100000";
  method_aern2_MP1_bparamss="$steps100 $steps1000 $steps10000 100000";

  #method_ireal_CR_bparamss="$steps100 1000 1330 1770 2370 3160 4210";
  # method_ireal_MP_bparamss="$steps100 $steps1000 $steps10000 100000";
  # method_ireal_MP1_bparamss="$steps100 $steps1000 $steps10000 100000";
  #method_ireal_MP2_bparamss="$steps100 $steps1000 $steps10000 100000";
  # method_aern2_MP_bparamss="$steps100 $steps1000 $steps10000 100000";
  #method_aern2_MP2_bparamss="$steps100 $steps1000 $steps10000 100000";

  bench="logistic"; dir="$bench";
  params="100";
  runForAllMethods
}

function logisticAllMethods
{
  method_cdar_bparamss="100 316 1000 3160 10000 31600 100000";
  #method_cdar_mBound_bparamss="100 316 1000 3160 10000 31600 100000";
  #method_ireal_CR_bparamss="100 316 1000 3160";
  method_aern2_CR_bparamss="100 316 1000 3160 10000 31600 100000";
  # method_ireal_MP_bparamss="100 316 1000 3160 10000 31600 100000";
  # method_ireal_MP1_bparamss="100 316 1000 3160 10000 31600 100000";
  #method_ireal_MP2_bparamss="100 316 1000 3160 10000 31600 100000";
  # method_aern2_MP_bparamss="100 316 1000 3160 10000 31600 100000";
  method_aern2_MP1_bparamss="100 316 1000 3160 10000 31600 100000";
  #method_aern2_MP2_bparamss="100 316 1000 3160 10000 31600 100000";

  bench="logistic"; dir="$bench";
  params="100";
  runForAllMethods
}

function manydigitsAllMethodsFine
{
  steps100="100 133 177 237 316 421 562 749"
  steps1000="1000 1330 1770 2370 3160 4210 5620 7490"
  steps10000="10000 13300 17700 23700 31600 42100 56200 74900"

  method_cdar_bparamss="$steps100 $steps1000 $steps10000 100000";
  method_aern2_CR_bparamss="$steps100 $steps1000 $steps10000 100000";
  # method_ireal_MP_bparamss="$steps100 $steps1000 $steps10000 100000";
  method_aern2_MP_bparamss="$steps100 $steps1000 $steps10000 100000";
  #method_cdar_bparamss="$steps100 $steps1000 10000";

  bench="manydigits$problem_number"; dir="$bench";
  params="100";
  runForAllMethods
}


# logisticAllMethods
logisticAllMethodsFine
problem_number=1
manydigitsAllMethodsFine
problem_number=2
manydigitsAllMethodsFine
problem_number=3
manydigitsAllMethodsFine
problem_number=4
manydigitsAllMethodsFine
problem_number=5
manydigitsAllMethodsFine
problem_number=7
manydigitsAllMethodsFine
#problem_number=8
#manydigitsAllMethodsFine
