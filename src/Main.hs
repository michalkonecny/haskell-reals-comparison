{-# LANGUAGE DataKinds #-}
module Main where

import Prelude
import Data.String (fromString)
import Control.Exception

import Data.Number.IReal (IReal) -- package ireal
--import Data.CReal (CReal) -- package exact-real
import qualified AERN2.Num as ANum 
    (CauchyReal, cauchyReal, 
     getAccuracy, bits,
     MPBall, rational2BallP, iterateUntilAccurate)
--import qualified AERN2.Net as ANet ()

import qualified Tasks.PreludeOps as TP
import qualified Tasks.AERN2Ops as TA
import System.Environment (getArgs)

main :: IO ()
main =
    do
    [benchArg, implArg] <- getArgs
    let (resultDecription, benchDecription) = bench benchArg implArg
    putStrLn benchDecription
    putStrLn resultDecription
    
    
bench :: String -> String -> (String, String)
bench benchArg implArg =
    (implArg ++ ": " ++ resultDecription, benchDecription)
    where
    (benchName, benchParams, benchDecription) =
        case benchArg of
            "logistic0" -> logisticAux 100
            "logistic1" -> logisticAux 1000 
            "logistic2" -> logisticAux 10000
            "logistic3" -> logisticAux 100000
            _ ->
                error $ "unknown benchmark: " ++ benchArg
        where
        logisticAux n = ("logistic", [n],  TP.taskLogisticDescription n)
    resultDecription =
        case (benchName, benchParams) of
            ("logistic", [n]) -> 
                case implArg of
                    "ireal" -> show (TP.taskLogistic n :: IReal)
--                    "exact-real" -> show (TP.taskLogistic n :: CReal 100)
                    "aern2_CR_preludeOps" -> show (TP.taskLogistic n :: ANum.CauchyReal)
                    "aern2_CR_aern2Ops" -> show (TA.taskLogistic n (ANum.cauchyReal TA.taskLogistic_x0))
                    "aern2_MP_aern2Ops" -> show (taskLogisticMP n)
                    _ -> error $ "unknown implementation: " ++ implArg
            _ -> error ""
     
    
taskLogisticMP :: Integer -> Maybe ANum.MPBall
taskLogisticMP n =
    snd $ last $ ANum.iterateUntilAccurate (ANum.bits (50 :: Integer)) $ \p ->
        TA.taskLogisticWithHook n checkAccuracy (ANum.rational2BallP p TA.taskLogistic_x0)
    
checkAccuracy :: ANum.MPBall -> Maybe ANum.MPBall
checkAccuracy ball 
    | ANum.getAccuracy ball < (ANum.bits 50) = Nothing 
    | otherwise = Just ball 
