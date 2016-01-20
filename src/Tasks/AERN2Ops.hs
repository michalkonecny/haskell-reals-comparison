{-# LANGUAGE Arrows, FlexibleContexts, TemplateHaskell, TypeOperators #-}
module Tasks.AERN2Ops where

import AERN2.Num
import Control.Category
--import Control.Arrow

logisticWithHookA :: (ArrowReal to r) => (r `to` r) -> Rational -> Integer -> r `to` r
logisticWithHookA hook c n =
    (foldl1 (<<<) (replicate (int n) step)) 
    where
    step = $(exprA[|let [x]=vars in  c * x * (1 - x)|]) >>> hook
    

    
taskLogistic0WithHook :: ArrowReal to r => r `to` r -> r `to` r
taskLogistic0WithHook hook = logisticWithHookA hook 3.82 100

taskLogistic0 :: ArrowReal to r => r `to` r
taskLogistic0 = taskLogistic0WithHook id

taskLogistic0x0 :: Rational
taskLogistic0x0 = 0.125     
    
taskLogistic1WithHook :: ArrowReal to r => r `to` r -> r `to` r
taskLogistic1WithHook hook = logisticWithHookA hook 3.82 1000

taskLogistic1 :: ArrowReal to r => r `to` r
taskLogistic1 = taskLogistic1WithHook id

taskLogistic1x0 :: Rational
taskLogistic1x0 = 0.125     
    
    
taskLogistic2WithHook :: ArrowReal to r => r `to` r -> r `to` r
taskLogistic2WithHook hook = logisticWithHookA hook 3.82 10000

taskLogistic2 :: ArrowReal to r => r `to` r
taskLogistic2 = taskLogistic2WithHook id

taskLogistic2x0 :: Rational
taskLogistic2x0 = 0.125     
    