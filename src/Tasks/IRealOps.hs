{-|
    Module      :  Tasks.IRealOps
    Copyright   :  (c) Björn von Sydow <bjorn.von.sydow@gmail.com>
    License     :  BSD3

    Maintainer  :  Michal Konecny <mikkonecny@gmail.com>
    Stability   :  experimental
-}

module Tasks.IRealOps where

import Prelude
import Tasks.PreludeOps (taskLogistic_c, taskLogistic_x0, logisticWithHook)
import Data.Number.IReal
import Data.Maybe

-- -- Precision in displayed IReal results is 30 decimals (chosen arbitrarily).
-- showPrecision :: Int
-- showPrecision = 30

-- Both taskLogistic functions here limit precision in the hook function,
-- forcing immediate evaluation at the given precision and thus imposing some strictness,
-- avoiding forming big unevaluated thunks.

-- Alt 1: Iteration of logisticWithHook with exponentially growing precision.
-- Start with showPrecision decimals, give up and double nr of decimals
-- as soon as intermediate value has too low precision.
taskLogistic1 :: Integer -> Int -> IReal
taskLogistic1 n showPrecision = loop showPrecision
  where loop d = maybe (loop (2*d)) id (logisticWithHook (hook d) taskLogistic_c n taskLogistic_x0)
        hook d x
          | rad r <! 0.1^showPrecision `atDecimals` (showPrecision+1) = Just r
          | otherwise = Nothing
          where r = prec d x

-- Alt 2: No iteration; exactly the required precision is used.
taskLogistic2 :: Integer -> Int -> IReal
taskLogistic2 n showPrecision =
    fromJust (logisticWithHook (Just . prec (precReq n showPrecision))
                                             taskLogistic_c
                                             n
                                             taskLogistic_x0)


-- Precision required in intermediate values for given n and d (nr of decimals displayed in final result).
precReq :: Integer -> Int -> Int
precReq n d = ceiling (fromInteger n * logBase 10 taskLogistic_c) + d
