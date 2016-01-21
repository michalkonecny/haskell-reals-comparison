{-# LANGUAGE Arrows, FlexibleContexts, TemplateHaskell, TypeOperators #-}
module Tasks.AERN2Ops where

import AERN2.Num
import Control.Category
import Control.Arrow
import Tasks.PreludeOps (taskLogistic_c)

logisticWithHookA :: (ArrowReal to r) => (r `to` Maybe r) -> Rational -> Integer -> r `to` Maybe r
logisticWithHookA hook c n =
    (foldl1 (<<<) (replicate (int n) step)) <<< arr Just 
    where
    step =
        proc maybeX ->
            case maybeX of
                Nothing -> returnA -< Nothing
                Just x -> 
                    hook <<< $(exprA[|let [x]=vars in  c * x * (1 - x)|]) -< x
    
taskLogisticWithHook :: ArrowReal to r => Integer -> r `to` Maybe r -> r `to` Maybe r
taskLogisticWithHook n hook = logisticWithHookA hook taskLogistic_c n

taskLogistic :: ArrowReal to r => Integer -> r `to` r
taskLogistic n = 
    proc x -> 
        do 
        (Just r) <- taskLogisticWithHook n (arr Just) -< x
        returnA -< r 

