module Tasks.PreludeOps where

import Prelude
-- import Data.String (fromString)

-- Alternative definition with much better space behaviour.
-- (by Björn von Sydow)
logisticWithHook :: Num t => (t -> Maybe t) -> t -> Integer -> t -> Maybe t
logisticWithHook hook c n0 x0 = loop n0 (Just x0)
  where loop _ Nothing = Nothing
        loop 0 x = x
        loop n (Just x) = seq x (loop (n-1) (hook $ c * x * (1-x)))

-- logisticWithHook :: Fractional t => (t -> Maybe t) -> t -> Integer -> t -> Maybe t
-- logisticWithHook hook c n x0 =
--     foldl1 (.) (replicate (fromInteger n) step) (Just x0)
--     where
--     step (Just x) = hook $ c * x * (1 - x)
--     step Nothing = Nothing


logistic :: Fractional t => t -> Integer -> t -> t
logistic c n x0 = case logisticWithHook Just c n x0 of Just r -> r; _ -> error ""

taskLogistic_x0 :: Fractional t => t
taskLogistic_x0 = 0.125

taskLogistic_c :: Fractional t => t
taskLogistic_c = 3.82 -- not dyadic
--standardC = 3.8203125 -- dyadic

showC :: String
showC = show (taskLogistic_c :: Double)

taskLogisticDescription :: Integer -> String
taskLogisticDescription n = "taskLogistic1: " ++ show n ++ " iterations of logistic map with c = " ++ showC ++ " and x0 = 0.125"

taskLogisticWithHook :: (Fractional t) => Integer -> (t -> Maybe t) ->  t -> t -> Maybe t
taskLogisticWithHook n hook c x0 =
    logisticWithHook hook c n x0

taskLogistic :: (Fractional t) => Integer -> t
taskLogistic n =
    logistic taskLogistic_c n taskLogistic_x0
