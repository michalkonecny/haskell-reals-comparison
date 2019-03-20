{-# LANGUAGE DataKinds, Arrows, FlexibleContexts, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import MixedTypesNumPrelude
import qualified Prelude as P
-- import Data.String (fromString)

import Control.Arrow hiding (loop)

import qualified Data.Number.IReal as IReal -- package ireal
import qualified Data.Number.IReal.IReal as IReal -- package ireal
import qualified Data.Number.IReal.IntegerInterval as IReal -- package ireal
import Data.Number.IReal (IReal)
import qualified Data.CDAR as CDAR

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
        case (take (length "manydigits") benchS, benchParams) of
            ("logistic", [n]) -> logisticAux n
            ("manydigits", [n]) -> 
              let problem_number = read $ drop (length "manydigits") benchS in
                manydigitsAux problem_number n
            _ ->
                error $ "unknown benchmark or incorrect number of parameters: " ++ benchS ++ show benchParams
        where
        logisticAux n = TP.taskLogisticDescription n
        manydigitsAux problem_number n = TP.taskManyDigitsDescription problem_number n
    resultDecription =
        case (take (length "manydigits") benchS, benchParams) of
            ("logistic", [n]) ->
                case implS of
                    "cdar" -> CDAR.showA . CDAR.limitSize (int acD) . CDAR.require (int acD) $ (TP.taskLogistic n)
                    "ireal_CR" -> IReal.showIReal (int acD) (TP.taskLogistic n)
                    "ireal_MP" -> show (taskLogisticIReal_TP n ac)
                    "ireal_MP1" -> show (TI.taskLogistic1 n (bits2dec $ fromAccuracy ac))
                    "ireal_MP2" -> show (TI.taskLogistic2 n (bits2dec $ fromAccuracy ac))
--                    "exact-real" -> show (TP.taskLogistic n :: CReal 100)
                    "aern2_CR_preludeOps" ->
                      show ((TP.taskLogistic n :: AERN2Real.CauchyReal) AERN2Real.? (accuracySG ac))
                    "aern2_MP_preludeOps" -> show (taskLogisticMP_TP n ac)
                    "aern2_MP" -> show (taskLogisticMP_TA n ac)
                    "aern2_MP1" -> show (taskLogisticMP1_TA n ac)
                    "aern2_MP2" -> show (taskLogisticMP2_TA n ac)
                    "aern2_CR" ->
                      show (taskLogisticCRpureArrow_TA n AERN2Real.? (accuracySG ac))
                    "aern2_CRcachedArrow" ->
                      show (taskLogisticCRcachedArrow_TA n ac)
                    _ -> error $ "unknown implementation: " ++ implS
            ("manydigits", [n]) ->
                let 
                  problem_number = read $ drop (length "manydigits") benchS
                  acND = n
                  acN = round ((acND) * 3.32)
                  acNA = bits acN
                  task :: (P.Floating t) => (Integer -> Maybe t) -> Maybe t -> (t -> Maybe t) -> Maybe t
                  task = TP.taskManyDigits problem_number
                  taskPrelude :: (P.Floating t) => t
                  taskPrelude = TP.taskManyDigitsSimple problem_number
                in
                case implS of
                    "cdar" -> CDAR.showA . CDAR.limitSize (int acN) . CDAR.require (int acN) $ taskPrelude
                    "aern2_CR" -> show $ (taskPrelude :: AERN2Real.CauchyReal) AERN2Real.? (accuracySG acNA)
                    "aern2_MP" -> show $ taskMBfromTask acNA task
                    "ireal_MP" -> IReal.showIReal (int acND) $ taskIRealfromTask acNA task
                    _ -> error $ "unknown implementation: " ++ implS
            _ -> error ""
    acD = round ((fromAccuracy ac) /! 3.32)

taskMBfromTask :: Accuracy -> ((Integer -> Maybe MPBall) -> Maybe MPBall -> (MPBall -> Maybe MPBall) -> Maybe MPBall) -> MPBall
taskMBfromTask ac task =
    case snd $ last $ MPBall.iterateUntilAccurate ac withP of
      Just r -> r
      _ -> error "taskMBfromTask failed"
    where
    withP p = 
      task (Just . mpBallP p) (Just $ MPBall.piBallP p) (Just . AERN2Real.setPrecision p)
    -- checkAccuracy

taskIRealfromTask :: Accuracy -> ((Integer -> Maybe IReal) -> Maybe IReal -> (IReal -> Maybe IReal) -> Maybe IReal) -> IReal
taskIRealfromTask ac task =
    case snd $ last $ MPBall.iterateUntilAccurate ac withP of
      Just r -> r
      _ -> error "taskIRealfromTask failed"
    where
    withP p = 
      task (setP . P.fromInteger) (setP P.pi) (setP)
      where
      precIR = bits2dec (integer p)
      setP = Just . IReal.prec precIR 

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

-- adaptation of Bjorn's code:
taskLogisticMP1_TA :: Integer -> Accuracy -> MPBall
taskLogisticMP1_TA n ac =
  loop (max 2 $ round $ 3.32 * fromAccuracy ac)
  where loop p =
          maybe (loop (2*p)) id (TA.taskLogisticWithHook n hook x0P)
          where
          x0P = mpBallP (AERN2Real.prec p) (TP.taskLogistic_x0 :: Rational)
        hook _ x
          | getAccuracy x >= ac = Just x
          | otherwise = Nothing

-- adaptation of Bjorn's code:
taskLogisticMP2_TA :: Integer -> Accuracy -> MPBall
taskLogisticMP2_TA n ac =
  TA.taskLogistic n x0P
      where
      x0P = mpBallP (AERN2Real.prec p) (TP.taskLogistic_x0 :: Rational)
      p = integer $ TI.logisticPrecReq2 n (int $ fromAccuracy ac)


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
