#!/bin/bash

resultscsv=$1
if [ "$resultscsv" == "" ]; then echo "usage: $0 <results csv file name>"; exit 1; fi

benchmain=haskell-reals-comparison

reuselogs="true"

# put headers in the results csv file if it is new:
if [ ! -f $resultscsv ]
    then echo "Time,Bench,BenchParams,Method,AccuracyTarget(bits),Accuracy(bits),UTime(s),STime(s),Mem(kB)" > $resultscsv
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
  echo "$now,$bench,$benchParams,$method,$params,$exact$bits,${utime/0.00/0.01},${stime/0.00/0.01},$mem" >> $resultscsv
}

function runForAllMethods
{
  if [ "$method_ireal_CR_bparamss" != "" ]; then
    method="ireal_CR"; bparamss="$method_ireal_CR_bparamss" method_ireal_CR_bparamss=""
    runForBenchParamss
  fi
  if [ "$method_ireal_MP_bparamss" != "" ]; then
    method="ireal_MP"; bparamss="$method_ireal_MP_bparamss" method_ireal_MP_bparamss=""
    runForBenchParamss
  fi
  # if [ "$method_aern2_CR_Prelude_bparamss" != "" ]; then
  #   method="aern2_CR_preludeOps"; bparamss="$method_aern2_CR_Prelude_bparamss" method_aern2_CR_Prelude_bparamss=""
  #   runForBenchParamss
  # fi
  # if [ "$method_aern2_MP_Prelude_bparamss" != "" ]; then
  #   method="aern2_MP_preludeOps"; bparamss="$method_aern2_MP_Prelude_bparamss" method_aern2_MP_Prelude_bparamss=""
  #   runForBenchParamss
  # fi
  if [ "$method_aern2_CR_bparamss" != "" ]; then
    method="aern2_CR"; bparamss="$method_aern2_CR_bparamss" method_aern2_CR_bparamss=""
    runForBenchParamss
  fi
  if [ "$method_aern2_MP_bparamss" != "" ]; then
    method="aern2_MP"; bparamss="$method_aern2_MP_bparamss" method_aern2_MP_bparamss=""
    runForBenchParamss
  fi
}

#################
### logistic
#################

function logisticAllMethods
{
  method_ireal_CR_bparamss="100 320 1000 3200";
  method_aern2_CR_bparamss="100 320 1000 3200 10000";
  method_ireal_MP_bparamss="100 320 1000 3200 10000 32000 100000";
  method_aern2_MP_bparamss="100 320 1000 3200 10000 32000 100000";

  bench="logistic"; dir="$bench";
  params="100";
  runForAllMethods
}


logisticAllMethods
