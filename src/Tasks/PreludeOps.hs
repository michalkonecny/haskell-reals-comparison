module Tasks.PreludeOps where

import Prelude
-- import Data.String (fromString)

import Text.Printf

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

taskManyDigitsSimple :: (Floating t) => Integer -> t
taskManyDigitsSimple problem_number =
    case taskManyDigits problem_number (Just . fromInteger) (Just pi) Just of
        Just r -> r
        _ -> error "taskManyDigitsSimple failed"

taskManyDigits :: (Floating t) => Integer -> (Integer -> Maybe t) -> (Maybe t) -> (t -> Maybe t) -> Maybe t
taskManyDigits problem_number fromI piT filterT =
    let f1 op x = (op <$> x) >>= filterT in
    let f2 op x y = (op <$> x <*> y) >>= filterT in
    case problem_number of
        1 -> f1 sin $ f1 tan $ f1 cos $ fromI 1
        2 -> f1 sqrt $ f2 (/) (f1 exp  $ fromI 1) piT
        3 -> f1 sin  $ f1 (^(3::Int)) $ f2 (+) (fromI 1) (f1 exp  $ fromI 1)
        4 -> f1 exp $ f2 (*) (f1 sqrt $ fromI 2011) piT
        5 -> f1 exp $ f1 exp $ f1 sqrt $ f1 exp $ fromI 1
        7 -> f1 (^(1000::Int)) piT
        8 -> f1 sin $ fromI $ 6^(6^(6 :: Int) :: Integer)
        _ -> error "Unknown/unimplemented many digits problem."
    
taskManyDigitsDescription :: Integer -> Integer -> String
taskManyDigitsDescription problem_number n =
    printf 
        ("Problem C%d with accuracy of %d decimal digits\n" ++ 
         " from the \"Many Digits\" competition\n" ++ 
         " (http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.307.7806)") 
        problem_number n