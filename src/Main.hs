{-# LANGUAGE DataKinds #-}
module Main where

import Prelude
import Data.String (fromString)

import Data.Number.IReal (IReal) -- package ireal
--import Data.CReal (CReal) -- package exact-real
import qualified AERN2.Num as ANum (CauchyReal, MPBall, bits, rational2BallP, cauchyReal, iterateUntilAccurate)
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
    benchDecription =
        case benchArg of
            "logistic0" -> 
                TP.taskLogistic0Description
            "logistic1" -> 
                TP.taskLogistic1Description
            "logistic2" ->
                TP.taskLogistic2Description
            _ ->
                error $ "unknown benchmark: " ++ benchArg
    resultDecription =
        case benchArg of
            "logistic0" -> 
                case implArg of
                    "ireal" -> show (TP.taskLogistic0 :: IReal)
--                    "exact-real" -> show (TP.taskLogistic0 :: CReal 100)
                    "aern2_CR_preludeOps" -> show (TP.taskLogistic0 :: ANum.CauchyReal)
                    "aern2_CR_aern2Ops" -> show (TA.taskLogistic0 (ANum.cauchyReal TA.taskLogistic1x0))
                    "aern2_MP_aern2Ops" -> show (taskLogistic0MP)
                    _ -> error $ "unknown implementation: " ++ implArg
            "logistic1" -> 
                case implArg of
                    "ireal" -> show (TP.taskLogistic1 :: IReal)
--                    "exact-real" -> show (TP.taskLogistic1 :: CReal 100)
                    "aern2_CR_preludeOps" -> show (TP.taskLogistic1 :: ANum.CauchyReal)
                    "aern2_CR_aern2Ops" -> show (TA.taskLogistic1 (ANum.cauchyReal TA.taskLogistic1x0))
                    "aern2_MP_aern2Ops" -> show (taskLogistic1MP)
                    _ -> error $ "unknown implementation: " ++ implArg
            "logistic2" -> 
                case implArg of
                    "ireal" -> show (TP.taskLogistic2 :: IReal)
--                    "exact-real" -> show (TP.taskLogistic2 :: CReal 100)
                    "aern2_CR_preludeOps" -> show (TP.taskLogistic2 :: ANum.CauchyReal)
                    "aern2_CR_aern2Ops" -> show (TA.taskLogistic2 (ANum.cauchyReal TA.taskLogistic2x0))
                    "aern2_MP_aern2Ops" -> show (taskLogistic2MP)
                    _ -> error $ "unknown implementation: " ++ implArg
            _ ->
                error $ "unknown benchmark: " ++ benchArg
     
    
taskLogistic0MP :: Maybe ANum.MPBall
taskLogistic0MP =
    snd $ last $ ANum.iterateUntilAccurate (ANum.bits (100 :: Integer)) $ \p ->
        TA.taskLogistic0 (ANum.rational2BallP p TA.taskLogistic0x0)
    
taskLogistic1MP :: Maybe ANum.MPBall
taskLogistic1MP =
    snd $ last $ ANum.iterateUntilAccurate (ANum.bits (100 :: Integer)) $ \p ->
        TA.taskLogistic1 (ANum.rational2BallP p TA.taskLogistic1x0)
    
taskLogistic2MP :: Maybe ANum.MPBall
taskLogistic2MP =
    snd $ last $ ANum.iterateUntilAccurate (ANum.bits (100 :: Integer)) $ \p ->
        TA.taskLogistic2 (ANum.rational2BallP p TA.taskLogistic2x0)
        