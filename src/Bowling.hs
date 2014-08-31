module Bowling (
    Frame,
    Frames,
    Roll,
    Rolls,
    RunningTotal,
    frameNumber,
    runningTotal,
    firstRoll,
    secondRoll,
    thirdRoll,
    processRolls,
    isLastFrame,
    isSpareFrame,
    isStrikeFrame,
    maxPins,
    maxFrames
    ) where

import Data.List
import qualified Data.Map as Map
import Data.Maybe

data FrameState
    = ReadyForFirstRoll
    | ReadyForSecondRoll
    | NeedBonusBalls
    | Complete
    deriving (Eq, Ord)

data Frame = Frame {
    fFrameNumber :: Int,
    fFrameState :: FrameState,
    fRunningTotal :: Maybe RunningTotal,
    fFirstRoll :: Maybe Roll,
    fSecondRoll :: Maybe Roll,
    fNumBonusBallsNeeded :: Int,
    fBonusBalls :: Rolls}

type Frames = [Frame]
type Roll = Int
type Rolls = [Roll]
type RunningTotal = Int

maxPins = 10
maxFrames = 10

frameNumber :: Frame -> Int
frameNumber = fFrameNumber

frameState :: Frame -> FrameState
frameState = fFrameState

runningTotal :: Frame -> Maybe RunningTotal
runningTotal = fRunningTotal

firstRoll :: Frame -> Maybe Roll
firstRoll = fFirstRoll

secondRoll :: Frame -> Maybe Roll
secondRoll f =
    case (isStrikeFrame f, isLastFrame f, not $ null $ fBonusBalls f) of
        (True, True, True) -> Just $ head $ fBonusBalls f
        otherwise -> fSecondRoll f

thirdRoll :: Frame -> Maybe Roll
thirdRoll f =
    case (isStrikeFrame f, isSpareFrame f, isLastFrame f, length $ fBonusBalls f) of
        (True, False, True, 2) -> Just $ last $ fBonusBalls f
        (False, True, True, 1) -> Just $ head $ fBonusBalls f
        otherwise -> Nothing

isSpareFrame :: Frame -> Bool
isSpareFrame f =
    r1 < maxPins && r1 + r2 == maxPins
    where
        r1 = fromMaybe 0 $ fFirstRoll f
        r2 = fromMaybe 0 $ fSecondRoll f

isStrikeFrame :: Frame -> Bool
isStrikeFrame f =
    case fFirstRoll f of
        Just x -> x == maxPins
        otherwise -> False

isLastFrame :: Frame -> Bool
isLastFrame f = fFrameNumber f == maxFrames

isStrikeRoll :: Roll -> Bool
isStrikeRoll roll = roll == maxPins

frameScore :: Frame -> Int
frameScore f =
    (fromMaybe 0 $ fFirstRoll f) +
    (fromMaybe 0 $ fSecondRoll f) +
    (sum $ fBonusBalls f)

twoRollsMakeSpare :: Frame -> Roll -> Bool
twoRollsMakeSpare f r = (fromMaybe 0 $ fFirstRoll f) + r == maxPins

calcNewRunningTotal :: Frame -> Roll -> Maybe RunningTotal -> Maybe RunningTotal
calcNewRunningTotal f r (Just rt) = Just $ frameScore f + r + rt
calcNewRunningTotal _ _ Nothing = Nothing

noChangeFrameState f _ = fFrameState f
noChangeRunningTotal f _ _ = fRunningTotal f
noChangeFirstRoll f _ = fFirstRoll f
noChangeSecondRoll f _ = fSecondRoll f
noChangeNumBonusBallsNeeded f _ = fNumBonusBallsNeeded f
noChangeBonusBalls f _ = fBonusBalls f

data StateMachineRow = StateMachineRow {
    stateFn :: Frame -> Roll -> FrameState,
    runningTotalFn :: Frame -> Roll -> Maybe RunningTotal -> Maybe RunningTotal,
    firstRollFn :: Frame -> Roll -> Maybe Roll,
    secondRollFn :: Frame -> Roll -> Maybe Roll,
    numBonusBallsNeededFn :: Frame -> Roll -> Int,
    bonusBallsFn :: Frame -> Roll -> Rolls,
    consumedBallFn :: Frame -> Roll -> Bool}

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
            firstRollFn = noChangeFirstRoll,
            secondRollFn = \_ r -> Just r,
            numBonusBallsNeededFn = \f r -> if twoRollsMakeSpare f r then 1 else 0,
            bonusBallsFn = \_ _ -> [],
            consumedBallFn = \_ _ -> True}),

        (NeedBonusBalls, StateMachineRow {
            stateFn = \f _ -> if fNumBonusBallsNeeded f == 1 then Complete else NeedBonusBalls,
            runningTotalFn = \f r rt -> if fNumBonusBallsNeeded f == 1 then calcNewRunningTotal f r rt else Nothing,            firstRollFn = noChangeFirstRoll,
            secondRollFn = noChangeSecondRoll,
            numBonusBallsNeededFn = \f _ -> pred $ fNumBonusBallsNeeded f,
            bonusBallsFn = \f r -> (fBonusBalls f) ++ [r],
            consumedBallFn = \f _ -> isLastFrame f}),

        (Complete, StateMachineRow {
            stateFn = noChangeFrameState,
            runningTotalFn = noChangeRunningTotal,
            firstRollFn = noChangeFirstRoll,
            secondRollFn = noChangeSecondRoll,
            numBonusBallsNeededFn = noChangeNumBonusBallsNeeded,
            bonusBallsFn = noChangeBonusBalls,
            consumedBallFn = \_ _ -> False})
    ]

applyRollToFrame :: Frame -> Roll -> Maybe RunningTotal -> (Frame, Bool, Maybe RunningTotal)
applyRollToFrame f r rt =
    (f', consumedBall, runningTotal f')
    where
        fs = fFrameState f
        smr = fromJust $ Map.lookup fs stateMachine
        f' = f {
            fFrameState = (stateFn smr) f r,
            fRunningTotal = (runningTotalFn smr) f r rt,
            fFirstRoll = (firstRollFn smr) f r,
            fSecondRoll = (secondRollFn smr) f r,
            fNumBonusBallsNeeded = (numBonusBallsNeededFn smr) f r,
            fBonusBalls = (bonusBallsFn smr) f r}
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
        initialFrames = [frameDefault { fFrameNumber = fn } | fn <- [1..maxFrames]]

frameDefault = Frame {
    fFrameNumber = undefined,
    fFrameState = ReadyForFirstRoll,
    fRunningTotal = Nothing,
    fFirstRoll = Nothing,
    fSecondRoll = Nothing,
    fNumBonusBallsNeeded = 0,
    fBonusBalls = []}
