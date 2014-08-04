module Bowling (
    Frame(..),
    RunningTotal,
    Roll,
    Rolls,
    Frames,
    processRolls,
    isLastFrame,
    maxPins,
    maxFrames
    ) where

import Data.List
import Data.Maybe
import Control.Monad (liftM)

maxPins = 10
maxFrames = 10

data FrameState
    = ReadyForFirstRoll
    | ReadyForSecondRoll
    | SpareNeedOneMore
    | StrikeNeedTwoMore
    | StrikeNeedOneMore
    | Complete
    deriving (Eq, Show)

data Frame = Frame {
    frameNumber :: Int,
    frameState :: FrameState,
    runningTotal :: Maybe RunningTotal,
    firstRoll :: Maybe Roll,
    secondRoll :: Maybe Roll,
    thirdRoll :: Maybe Roll,
    bonusBalls :: [Roll]}
    deriving Show

type RunningTotal = Int
type Roll = Int
type Rolls = [Roll]
type Frames = [Frame]

isStrike :: Roll -> Bool
isStrike roll = roll == maxPins

isLastFrame :: Frame -> Bool
isLastFrame f = frameNumber f == maxFrames

frameScore :: Frame -> Int
frameScore f =
    (fromMaybe 0 $ firstRoll f) +
    (fromMaybe 0 $ secondRoll f) +
    (fromMaybe 0 $ thirdRoll f) +
    (sum $ bonusBalls f)

applyRollToFrame :: Frame -> Roll -> Maybe RunningTotal -> (Frame, Bool, Maybe RunningTotal)
applyRollToFrame f roll rt = case frameState f of

    ReadyForFirstRoll ->
        let
            frameState' = if isStrike roll then StrikeNeedTwoMore else ReadyForSecondRoll
            f' = f { 
                frameState = frameState', 
                firstRoll = Just roll}
        in
            (f', True, Nothing)

    ReadyForSecondRoll ->
        let
            newScore = frameScore f + roll
            isSpare = newScore == maxPins
            frameState' = if isSpare then SpareNeedOneMore else Complete
            rt' = rt >>= (\x -> if isSpare then Nothing else Just (x + newScore))
            f' = f { 
                frameState = frameState', 
                runningTotal = rt',
                secondRoll = Just roll}
        in
            (f', True, rt')

    SpareNeedOneMore ->
        let
            consumedBall = isLastFrame f
            thirdRoll' = if consumedBall then Just roll else Nothing
            bonusBalls' = if consumedBall then [] else [roll]
            frameState' = Complete
            newScore = frameScore f + roll
            rt' = (+newScore) `liftM` rt
            f' = f { 
                frameState = frameState', 
                runningTotal = rt',
                thirdRoll = thirdRoll',
                bonusBalls = bonusBalls'}
        in
            (f', consumedBall, rt')

    StrikeNeedTwoMore ->
        let
            consumedBall = isLastFrame f
            secondRoll' = if consumedBall then Just roll else Nothing
            bonusBalls' = if consumedBall then [] else [roll]
            frameState' = StrikeNeedOneMore
            f' = f { 
                frameState = frameState', 
                secondRoll = secondRoll',
                bonusBalls = bonusBalls'}
        in
            (f', consumedBall, Nothing)

    StrikeNeedOneMore ->
        let
            consumedBall = isLastFrame f
            thirdRoll' = if consumedBall then Just roll else Nothing
            bonusBalls' = if consumedBall then [] else (bonusBalls f) ++ [roll]
            frameState' = Complete
            newScore = frameScore f + roll
            rt' = (+newScore) `liftM` rt
            f' = f { 
                frameState = frameState', 
                runningTotal = rt',
                thirdRoll = thirdRoll',
                bonusBalls = bonusBalls'}
        in
            (f', consumedBall, rt')

    Complete -> (f, False, (+ frameScore f) `liftM` rt)

fstOfTriple :: (a, b, c) -> a
fstOfTriple (a, _, _) = a

processRoll :: Frames -> Roll -> Frames
processRoll fs roll =
    reverse . fstOfTriple $ foldl op ([], False, Just 0) fs
    where
        op (fs', consumedBall, rt) f =
            if consumedBall then
                (f : fs', consumedBall, rt)
            else
                (f' : fs', consumedBall', rt')
            where
                (f', consumedBall', rt') = applyRollToFrame f roll rt

processRolls :: Rolls -> Frames
processRolls rolls =
    foldl processRoll initialFrames rolls
    where
        initialFrames = [frameDefault { frameNumber = fn } | fn <- [1..maxFrames]]
        frameDefault = Frame {
            frameNumber = undefined,
            frameState = ReadyForFirstRoll,
            runningTotal = Nothing,
            firstRoll = Nothing,
            secondRoll = Nothing,
            thirdRoll = Nothing,
            bonusBalls = []}
