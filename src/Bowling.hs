module Bowling (
    Frame(..),
    RunningTotal,
    Roll,
    Rolls,
    Frames,
    processRolls,
    maxPins,
    maxFrames,
    frameDefault,
    isLastFrame,
    isSpareFrame,
    isStrikeFrame
    ) where

import Data.List
import qualified Data.Map as Map
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
    deriving (Eq, Show, Ord)

data Frame = Frame {
    frameNumber :: Int,
    frameState :: FrameState,
    runningTotal :: Maybe RunningTotal,
    firstRoll :: Maybe Roll,
    secondRoll :: Maybe Roll,
    bonusBalls :: Rolls}
    deriving Show

type RunningTotal = Int
type Roll = Int
type Rolls = [Roll]
type Frames = [Frame]

isSpareFrame :: Frame -> Bool
isSpareFrame f =
    r1 < maxPins && r1 + r2 == maxPins
    where
        r1 = (fromMaybe 0 $ firstRoll f)
        r2 = (fromMaybe 0 $ secondRoll f)

isStrikeFrame :: Frame -> Bool
isStrikeFrame f =
    case firstRoll f of
        Just x -> x == maxPins
        otherwise -> False

isLastFrame :: Frame -> Bool
isLastFrame f = frameNumber f == maxFrames

isStrikeRoll :: Roll -> Bool
isStrikeRoll roll = roll == maxPins

frameScore :: Frame -> Int
frameScore f =
    (fromMaybe 0 $ firstRoll f) +
    (fromMaybe 0 $ secondRoll f) +
    (sum $ bonusBalls f)

data StateMachineRow = StateMachineRow {
    stateFn :: Frame -> Roll -> FrameState,
    runningTotalFn :: Frame -> Roll -> Maybe RunningTotal,
    firstRollFn :: Frame -> Roll -> Maybe Roll,
    secondRollFn :: Frame -> Roll -> Maybe Roll,
    bonusBallsFn :: Frame -> Roll -> Rolls,
    consumedBallFn :: Frame -> Roll -> Bool}

stateMachine = Map.fromList [

        (ReadyForFirstRoll, StateMachineRow {
            stateFn = \_ r -> if isStrikeRoll r then StrikeNeedTwoMore else ReadyForSecondRoll,
            runningTotalFn = \_ _ -> Nothing,
            firstRollFn = \f r -> Just r,
            secondRollFn = \f _ -> Nothing,
            bonusBallsFn = \f _ -> [],
            consumedBallFn = \f _ -> True}),

        (ReadyForSecondRoll, StateMachineRow {
            stateFn = \_ _ -> Complete, -- TODO
            runningTotalFn = \f _ -> runningTotal f, -- TODO
            firstRollFn = \f _ -> firstRoll f,
            secondRollFn = \f r -> Just r,
            bonusBallsFn = \f _ -> [],
            consumedBallFn = \f _ -> True}),

        (SpareNeedOneMore, StateMachineRow {
            stateFn = \_ _ -> Complete,
            runningTotalFn = \f _ -> runningTotal f, -- TODO
            firstRollFn = \f _ -> firstRoll f,
            secondRollFn = \f _ -> secondRoll f,
            bonusBallsFn = \f r -> (bonusBalls f) ++ [r],
            consumedBallFn = \f _ -> isLastFrame f}),

        (StrikeNeedTwoMore, StateMachineRow {
            stateFn = \_ _ -> StrikeNeedOneMore,
            runningTotalFn = \_ _ -> Nothing,
            firstRollFn = \f _ -> firstRoll f,
            secondRollFn = \f _ -> secondRoll f,
            bonusBallsFn = \f r -> (bonusBalls f) ++ [r],
            consumedBallFn = \f _ -> isLastFrame f}),

        (StrikeNeedOneMore, StateMachineRow {
            stateFn = \_ _ -> Complete,
            runningTotalFn = \f _ -> runningTotal f, -- TODO
            firstRollFn = \f _ -> firstRoll f,
            secondRollFn = \f _ -> secondRoll f,
            bonusBallsFn = \f r -> (bonusBalls f) ++ [r],
            consumedBallFn = \f _ -> isLastFrame f}),

        (Complete, StateMachineRow {
            stateFn = \f _ -> frameState f,
            runningTotalFn = \f _ -> runningTotal f,
            firstRollFn = \f _ -> firstRoll f,
            secondRollFn = \f _ -> secondRoll f,
            bonusBallsFn = \f _ -> bonusBalls f,
            consumedBallFn = \f _ -> False})
    ]

applyRollToFrame :: Frame -> Roll -> Maybe RunningTotal -> (Frame, Bool, Maybe RunningTotal)

applyRollToFrame f@Frame {frameState = ReadyForFirstRoll} roll rt =
    (f', True, Nothing)
    where
        frameState' = if isStrikeRoll roll then StrikeNeedTwoMore else ReadyForSecondRoll
        f' = f {
            frameState = frameState',
            firstRoll = Just roll}

applyRollToFrame f@Frame {frameState = ReadyForSecondRoll} roll rt =
    (f', True, rt')
    where
        newScore = frameScore f + roll
        isSpare = newScore == maxPins
        frameState' = if isSpare then SpareNeedOneMore else Complete
        rt' = rt >>= (\x -> if isSpare then Nothing else Just (x + newScore))
        f' = f {
            frameState = frameState',
            runningTotal = rt',
            secondRoll = Just roll}

applyRollToFrame f@Frame {frameState = SpareNeedOneMore} roll rt =
    (f', consumedBall, rt')
    where
        consumedBall = isLastFrame f
        bonusBalls' = (bonusBalls f) ++ [roll]
        frameState' = Complete
        newScore = frameScore f + roll
        rt' = (+newScore) `liftM` rt
        f' = f {
            frameState = frameState',
            runningTotal = rt',
            bonusBalls = bonusBalls'}

applyRollToFrame f@Frame {frameState = StrikeNeedTwoMore} roll rt =
    (f', consumedBall, Nothing)
    where
        consumedBall = isLastFrame f
        bonusBalls' = (bonusBalls f) ++ [roll]
        frameState' = StrikeNeedOneMore
        f' = f {
            frameState = frameState',
            bonusBalls = bonusBalls'}

applyRollToFrame f@Frame {frameState = StrikeNeedOneMore} roll rt =
    (f', consumedBall, rt')
    where
        consumedBall = isLastFrame f
        bonusBalls' = (bonusBalls f) ++ [roll]
        frameState' = Complete
        newScore = frameScore f + roll
        rt' = (+newScore) `liftM` rt
        f' = f {
            frameState = frameState',
            runningTotal = rt',
            bonusBalls = bonusBalls'}

applyRollToFrame f@Frame {frameState = Complete} roll rt =
    (f, False, newScore)
    where
        newScore = (+ frameScore f) `liftM` rt

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

frameDefault = Frame {
    frameNumber = undefined,
    frameState = ReadyForFirstRoll,
    runningTotal = Nothing,
    firstRoll = Nothing,
    secondRoll = Nothing,
    bonusBalls = []}

processRolls :: Rolls -> Frames
processRolls rolls =
    foldl processRoll initialFrames rolls
    where
        initialFrames = [frameDefault { frameNumber = fn } | fn <- [1..maxFrames]]
