{-# LANGUAGE DataKinds, Arrows, FlexibleContexts, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import AERN2.MP.Ball (MPBall, mpBallP)
import AERN2.MP.Accuracy
import AERN2.AccuracySG

import qualified AERN2.Real as AERN2Real
import AERN2.QA.Protocol ((-:-))
import AERN2.QA.Strategy.Cached (executeQACachedA)

import qualified Tasks.PreludeOps as TP
import qualified Tasks.MixedTypesNumOps as TA
import qualified Tasks.IRealOps as TI
import System.Environment (getArgs)

main :: IO ()
main =
    do
    [benchS,  benchParamsS, implS, acS] <- getArgs
    let benchParams = read ("[" ++ benchParamsS ++ "]") :: [Integer]
    let ac = bits (read acS :: Integer)
    let (resultDecription, benchDecription) = bench benchS benchParams implS ac
    putStrLn benchDecription
    putStrLn resultDecription

bench :: String -> [Integer] -> String -> Accuracy -> (String, String)
bench benchS benchParams implS ac =
    (implS ++ ": " ++ resultDecription, benchDecription)
    where
    benchDecription =
        case (benchS, benchParams) of
            ("logistic", [n]) -> logisticAux n
            _ ->
                error $ "unknown benchmark: " ++ benchS
        where
        logisticAux n = TP.taskLogisticDescription n
    resultDecription =
        case (benchS, benchParams) of
            ("logistic", [n]) ->
                case implS of
                    "ireal_CR" -> IReal.showIReal (int acD) (TP.taskLogistic n)
                    "ireal_MP" -> show (taskLogisticIReal_TP n ac)
                    "ireal_MP1" -> show (TI.taskLogistic1 n (bits2dec $ fromAccuracy ac))
                    "ireal_MP2" -> show (TI.taskLogistic2 n (bits2dec $ fromAccuracy ac))
--                    "exact-real" -> show (TP.taskLogistic n :: CReal 100)
                    "aern2_CR_preludeOps" ->
                      show ((TP.taskLogistic n :: AERN2Real.CauchyReal) AERN2Real.? (accuracySG ac))
                    "aern2_MP_preludeOps" -> show (taskLogisticMP_TP n ac)
                    "aern2_MP" -> show (taskLogisticMP_TA n ac)
                    "aern2_CR" ->
                      show (taskLogisticCRpureArrow_TA n AERN2Real.? (accuracySG ac))
                    "aern2_CRcachedArrow" ->
                      show (taskLogisticCRcachedArrow_TA n ac)
                    _ -> error $ "unknown implementation: " ++ implS
            _ -> error ""
    acD = round ((fromAccuracy ac) /! 3.32)

taskLogisticCRpureArrow_TA :: Integer -> AERN2Real.CauchyReal
taskLogisticCRpureArrow_TA n =
  TA.taskLogistic n $ AERN2Real.real (TP.taskLogistic_x0 :: Rational)

taskLogisticCRcachedArrow_TA :: Integer -> Accuracy -> MPBall
taskLogisticCRcachedArrow_TA n ac =
  snd $ executeQACachedA $
    proc () ->
      do
      x0R <- (-:-)-< AERN2Real.realA x0
      (Just x) <-TA.taskLogisticWithHookA n hookA -< x0R
      AERN2Real.realWithAccuracyA Nothing -< (x, accuracySG ac)
  where
  x0 = TP.taskLogistic_x0 :: Rational
  hookA i =
    proc r ->
      do
      rNext <- (-:-)-< (rename r)
      returnA -< Just rNext
    where
    rename = AERN2Real.realRename (\_ -> "x_" ++ show i)

taskLogisticMP_TP :: Integer -> Accuracy -> Maybe MPBall
taskLogisticMP_TP n ac =
    snd $ last $ MPBall.iterateUntilAccurate ac withP
    where
    withP p =
        TP.taskLogisticWithHook n checkAccuracyMP c x0
        where
        x0 = mpBallP p (TP.taskLogistic_x0 :: Rational)
        c = mpBallP p (TP.taskLogistic_c :: Rational)

taskLogisticMP_TA :: Integer -> Accuracy -> Maybe MPBall
taskLogisticMP_TA n ac =
    snd $ last $ MPBall.iterateUntilAccurate ac withP
    where
    withP p =
        (TA.taskLogisticWithHook n (const checkAccuracyMP)) x0
        where
        x0 = mpBallP p (TP.taskLogistic_x0 :: Rational)


checkAccuracyMP :: MPBall -> Maybe MPBall
checkAccuracyMP ball
    | MPBall.getAccuracy ball < (MPBall.bits 100) = Nothing
    | otherwise = Just ball

taskLogisticIReal_TP :: Integer -> Accuracy -> Maybe IReal
taskLogisticIReal_TP n ac =
    snd $ last $ AERN2Real.iterateUntilAccurate ac $ withP
    where
    withP p =
        TP.taskLogisticWithHook n (setPAndCheckAccuracyIReal ac p) c x0
        where
        x0 = P.fromRational (TP.taskLogistic_x0 :: Rational)
        c = P.fromRational (TP.taskLogistic_c :: Rational)

setPAndCheckAccuracyIReal :: Accuracy -> AERN2Real.Precision -> IReal -> Maybe IReal
setPAndCheckAccuracyIReal ac p rB
  | AERN2Real.getAccuracy rP < ac = Nothing
  | otherwise = Just rP
  where
  rP = IReal.prec (bits2dec (integer p)) rB

bits2dec :: Integer -> Int
bits2dec b = int $ round $ b /! 3.32

instance AERN2Real.HasAccuracy IReal where
  getAccuracy rB
    | rdIsZero = AERN2Real.Exact
    | otherwise =
        AERN2Real.bits $ IReal.lg2 $ P.ceiling $ (P.recip rd P.+ (P.fromRational 0.5))
    where
    rd = IReal.rad rB
    rdIsZero = 1 == (IReal.midI $ IReal.appr rd (int 10000000)) -- not entirely safe...
