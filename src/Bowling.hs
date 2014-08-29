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

maxPins = 10
maxFrames = 10

data FrameState
    = ReadyForFirstRoll
    | ReadyForSecondRoll
    | NeedBonusBalls
    | Complete
    deriving (Eq, Show, Ord)

data Frame = Frame {
    frameNumber :: Int,
    frameState :: FrameState,
    runningTotal :: Maybe RunningTotal,
    firstRoll :: Maybe Roll,
    secondRoll :: Maybe Roll,
    numBonusBallsNeeded :: Int,
    bonusBalls :: Rolls}
    deriving Show

frameDefault = Frame {
    frameNumber = undefined,
    frameState = ReadyForFirstRoll,
    runningTotal = Nothing,
    firstRoll = Nothing,
    secondRoll = Nothing,
    numBonusBallsNeeded = 0,
    bonusBalls = []}

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
    runningTotalFn :: Frame -> Roll -> Maybe RunningTotal -> Maybe RunningTotal,
    firstRollFn :: Frame -> Roll -> Maybe Roll,
    secondRollFn :: Frame -> Roll -> Maybe Roll,
    bonusBallsFn :: Frame -> Roll -> Rolls,
    numBonusBallsNeededFn :: Frame -> Roll -> Int,
    consumedBallFn :: Frame -> Roll -> Bool}

twoRollsMakeSpare :: Frame -> Roll -> Bool
twoRollsMakeSpare f r = (fromMaybe 0 $ firstRoll f) + r == maxPins

calcNewRunningTotal :: Frame -> Roll -> Maybe RunningTotal -> Maybe RunningTotal
calcNewRunningTotal f r (Just rt) = Just $ frameScore f + r + rt
calcNewRunningTotal _ _ Nothing = Nothing

noChangeFrameState = frameState
noChangeRunningTotal = runningTotal
noChangeFirstRoll = firstRoll
noChangeSecondRoll = secondRoll
noChangeNumBonusBallsNeeded = numBonusBallsNeeded
noChangeBonusBalls = bonusBalls

stateMachine = Map.fromList [

        (ReadyForFirstRoll, StateMachineRow {
            stateFn = \_ r -> if isStrikeRoll r then NeedBonusBalls else ReadyForSecondRoll,
            runningTotalFn = \_ _ _ -> Nothing,
            firstRollFn = \_ r -> Just r,
            secondRollFn = \_ _ -> Nothing,
            numBonusBallsNeededFn = \_ r -> if isStrikeRoll r then 2 else 0,
            bonusBallsFn = \_ _ -> [],
            consumedBallFn = \_ _ -> True}),

        (ReadyForSecondRoll, StateMachineRow {
            stateFn = \f r -> if twoRollsMakeSpare f r then NeedBonusBalls else Complete,
            runningTotalFn = \f r rt -> if twoRollsMakeSpare f r then Nothing else calcNewRunningTotal f r rt, 
            firstRollFn = \f _ -> noChangeFirstRoll f,
            secondRollFn = \_ r -> Just r,
            numBonusBallsNeededFn = \f r -> if twoRollsMakeSpare f r then 1 else 0,
            bonusBallsFn = \_ _ -> [],
            consumedBallFn = \_ _ -> True}),

        (NeedBonusBalls, StateMachineRow {
            stateFn = \f _ -> if numBonusBallsNeeded f == 1 then Complete else NeedBonusBalls,
            runningTotalFn = \f r rt -> if numBonusBallsNeeded f == 1 then calcNewRunningTotal f r rt else Nothing,            firstRollFn = \f _ -> noChangeFirstRoll f,
            secondRollFn = \f _ -> noChangeSecondRoll f,
            numBonusBallsNeededFn = \f _ -> (numBonusBallsNeeded f) - 1,
            bonusBallsFn = \f r -> (bonusBalls f) ++ [r],
            consumedBallFn = \f _ -> isLastFrame f}),

        (Complete, StateMachineRow {
            stateFn = \f _ -> noChangeFrameState f,
            runningTotalFn = \f _ _ -> noChangeRunningTotal f,
            firstRollFn = \f _ -> noChangeFirstRoll f,
            secondRollFn = \f _ -> noChangeSecondRoll f,
            numBonusBallsNeededFn = \f _ -> noChangeNumBonusBallsNeeded f,
            bonusBallsFn = \f _ -> noChangeBonusBalls f,
            consumedBallFn = \f _ -> False})
    ]

applyRollToFrame :: Frame -> Roll -> Maybe RunningTotal -> (Frame, Bool, Maybe RunningTotal)

applyRollToFrame f r rt =
    (f', consumedBall, runningTotal f')
    where
        fs = frameState f
        smr = fromJust $ Map.lookup fs stateMachine
        f' = f {
            frameState = (stateFn smr) f r,
            runningTotal = (runningTotalFn smr) f r rt,
            firstRoll = (firstRollFn smr) f r,
            secondRoll = (secondRollFn smr) f r,
            numBonusBallsNeeded = (numBonusBallsNeededFn smr) f r,
            bonusBalls = (bonusBallsFn smr) f r}
        consumedBall = (consumedBallFn smr) f r

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
