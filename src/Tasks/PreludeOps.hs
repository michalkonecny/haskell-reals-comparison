module Tasks.PreludeOps where

import Prelude
import Data.String (fromString)

logistic :: Fractional t => t -> Int -> t -> t
logistic c n x0 =
    foldl1 (.) (replicate n step) x0
    where
    step x = c * x * (1 - x)

taskLogistic0Description :: String
taskLogistic0Description = "taskLogistic1: 100 iterations of logistic map with c = 3.82 and x0 = 0.125"

taskLogistic0 :: (Fractional t) => t
taskLogistic0 =
    logistic 3.82 100 0.125

taskLogistic1Description :: String
taskLogistic1Description = "taskLogistic1: 1000 iterations of logistic map with c = 3.82 and x0 = 0.125"

taskLogistic1 :: (Fractional t) => t
taskLogistic1 =
    logistic 3.82 1000 0.125

taskLogistic2Description :: String
taskLogistic2Description = "taskLogistic1: 10000 iterations of logistic map with c = 3.82 and x0 = 0.125"

taskLogistic2 :: (Fractional t) => t
taskLogistic2 =
    logistic 3.82 10000 0.125
