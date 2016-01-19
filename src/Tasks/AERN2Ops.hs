{-# LANGUAGE Arrows, FlexibleContexts, TemplateHaskell, TypeOperators #-}
module Tasks.AERN2Ops where

import AERN2.Num
import Control.Category
--import Control.Arrow

logisticA :: ArrowReal to t => Rational -> Integer -> t `to` t
logisticA c n =
    foldl1 (<<<) (replicate (int n) stepA)
    where
    stepA = $(exprA[| let [x] = vars in c * x * (1 - x) |])
    
taskLogistic0 :: ArrowReal to t => t `to` t
taskLogistic0 = logisticA 3.82 100

taskLogistic0x0 :: Rational
taskLogistic0x0 = 0.125     
    
taskLogistic1 :: ArrowReal to t => t `to` t
taskLogistic1 = logisticA 3.82 1000

taskLogistic1x0 :: Rational
taskLogistic1x0 = 0.125     
    
taskLogistic2 :: ArrowReal to t => t `to` t
taskLogistic2 = logisticA 3.82 10000

taskLogistic2x0 :: Rational
taskLogistic2x0 = 0.125     

