{-# LANGUAGE FlexibleContexts, TypeOperators, TypeFamilies, ConstraintKinds #-}
module Tasks.AERN2Ops where

import Numeric.MixedTypes

import Control.Monad

import Tasks.PreludeOps (taskLogistic_c)

logisticWithHook :: (Ring r, CanMulBy r Rational) => (r -> Maybe r) -> Rational -> Integer -> r -> Maybe r
logisticWithHook hook c n =
    foldl1 (<=<) (replicate (int n) step)
    where
    step x =
      hook $ c * x * (1 - x)

taskLogisticWithHook :: (Ring r, CanMulBy r Rational) => Integer -> (r -> Maybe r) -> r -> Maybe r
taskLogisticWithHook n hook = logisticWithHook hook taskLogistic_c n

taskLogistic :: (Ring r, CanMulBy r Rational) => Integer -> r -> r
taskLogistic n x = r
  where
  (Just r) = taskLogisticWithHook n Just x
