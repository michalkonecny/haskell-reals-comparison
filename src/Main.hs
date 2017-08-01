{-# LANGUAGE DataKinds, Arrows, FlexibleContexts, TypeFamilies #-}
module Main where

import MixedTypesNumPrelude
import qualified Prelude as P
-- import Data.String (fromString)

import Control.Arrow

import qualified Data.Number.IReal as IReal -- package ireal
import qualified Data.Number.IReal.IReal as IReal -- package ireal
import qualified Data.Number.IReal.IntegerInterval as IReal -- package ireal
import Data.Number.IReal (IReal)

--import Data.CReal (CReal) -- package exact-real

import qualified AERN2.MP.Ball as MPBall
    (
      -- CauchyReal, cauchyReal,
     bits, getAccuracy, Accuracy(..),
     iterateUntilAccurate)
import AERN2.MP.Ball (MPBall, mpBallP)

import qualified AERN2.Real as AERN2Real
import AERN2.QA.Protocol ((-:-))
import AERN2.QA.Strategy.Cached (executeQACachedA)

import qualified Tasks.PreludeOps as TP
import qualified Tasks.MixedTypesNumOps as TA
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
        logisticAux n = ("logistic"  :: String, [n],  TP.taskLogisticDescription n)
    resultDecription =
        case (benchName, benchParams) of
            ("logistic", [n]) ->
                case implArg of
                    "ireal_CR" -> show (TP.taskLogistic n :: IReal)
                    "ireal_MP" -> show (taskLogisticIReal_TP n)
--                    "exact-real" -> show (TP.taskLogistic n :: CReal 100)
                    -- "aern2_CR_preludeOps" -> show (TP.taskLogistic n :: AERN2Real.CauchyReal)
                    "aern2_MP_preludeOps" -> show (taskLogisticMP_TP n)
                    "aern2_MP_aern2Ops" -> show (taskLogisticMP_TA n)
                    "aern2_CR_aern2Ops" -> show (taskLogisticCRpureArrow_TA n)
                    "aern2_CRcachedArrow_aern2Ops" -> show (taskLogisticCRcachedArrow_TA n)
                    _ -> error $ "unknown implementation: " ++ implArg
            _ -> error ""

taskLogisticCRpureArrow_TA :: Integer -> AERN2Real.CauchyReal
taskLogisticCRpureArrow_TA n =
  TA.taskLogistic n $ AERN2Real.real (TP.taskLogistic_x0 :: Rational)

taskLogisticCRcachedArrow_TA :: Integer -> MPBall
taskLogisticCRcachedArrow_TA n =
  snd $ executeQACachedA $
    proc () ->
      do
      x0R <- (-:-)-< AERN2Real.realA x0
      (Just x) <-TA.taskLogisticWithHookA n hookA -< x0R
      AERN2Real.realWithAccuracyA Nothing -< (x, AERN2Real.bitsSG 100 120)
  where
  x0 = TP.taskLogistic_x0 :: Rational
  hookA i =
    proc r ->
      do
      rNext <- (-:-)-< (rename r)
      returnA -< Just rNext
    where
    rename = AERN2Real.realRename (\_ -> "x_" ++ show i)

taskLogisticMP_TP :: Integer -> Maybe MPBall
taskLogisticMP_TP n =
    snd $ last $ MPBall.iterateUntilAccurate (MPBall.bits (100 :: Integer)) $ withP
    where
    withP p =
        TP.taskLogisticWithHook n checkAccuracyMP c x0
        where
        x0 = mpBallP p (TP.taskLogistic_x0 :: Rational)
        c = mpBallP p (TP.taskLogistic_c :: Rational)

taskLogisticMP_TA :: Integer -> Maybe MPBall
taskLogisticMP_TA n =
    snd $ last $ MPBall.iterateUntilAccurate (MPBall.bits (100 :: Integer)) $ withP
    where
    withP p =
        (TA.taskLogisticWithHook n (const checkAccuracyMP)) x0
        where
        x0 = mpBallP p (TP.taskLogistic_x0 :: Rational)


checkAccuracyMP :: MPBall -> Maybe MPBall
checkAccuracyMP ball
    | MPBall.getAccuracy ball < (MPBall.bits 100) = Nothing
    | otherwise = Just ball

taskLogisticIReal_TP :: Integer -> Maybe IReal
taskLogisticIReal_TP n =
    snd $ last $ AERN2Real.iterateUntilAccurate (AERN2Real.bits (100 :: Integer)) $ withP
    where
    withP p =
        TP.taskLogisticWithHook n (setPAndCheckAccuracyIReal p) c x0
        where
        x0 = P.fromRational (TP.taskLogistic_x0 :: Rational)
        c = P.fromRational (TP.taskLogistic_c :: Rational)

setPAndCheckAccuracyIReal :: AERN2Real.Precision -> IReal -> Maybe IReal
setPAndCheckAccuracyIReal p rB
  | AERN2Real.getAccuracy rP < 100 = Nothing
  | otherwise = Just rP
  where
  rP = IReal.prec (int $ round $ (integer p) /! 3.32) rB

instance AERN2Real.HasAccuracy IReal where
  getAccuracy rB
    | rdIsZero = AERN2Real.Exact
    | otherwise =
        AERN2Real.bits $ IReal.lg2 $ P.ceiling $ (P.recip rd P.+ (P.fromRational 0.5))
    where
    rd = IReal.rad rB
    rdIsZero = 1 == (IReal.midI $ IReal.appr rd (int 10000000)) -- not entirely safe...
