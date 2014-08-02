module Bowling (
    Frame(..),
    processRolls,
    maxPins,
    maxFrames
    ) where

import Data.List
import Data.Maybe

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
    runningTotal :: Maybe Int,
    firstRoll :: Maybe Int,
    secondRoll :: Maybe Int,
    thirdRoll :: Maybe Int,
    bonusBalls :: [Int]}
    deriving Show

isStrike :: Int -> Bool
isStrike roll = roll == maxPins

isLastFrame :: Frame -> Bool
isLastFrame f = frameNumber f == maxFrames

frameScore :: Frame -> Int
frameScore f =
    (fromMaybe 0 $ firstRoll f) +
    (fromMaybe 0 $ secondRoll f) +
    (fromMaybe 0 $ thirdRoll f) +
    (sum $ bonusBalls f)

applyRollToFrame :: Frame -> Int -> Maybe Int -> (Frame, Bool, Maybe Int)
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
            newScore = (fromJust $ firstRoll f) + roll
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
            rt' = rt >>= (\x -> Just (x + newScore))
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
            rt' = rt >>= (\x -> Just (x + newScore))
            f' = f { 
                frameState = frameState', 
                runningTotal = rt',
                thirdRoll = thirdRoll',
                bonusBalls = bonusBalls'}
        in
            (f', consumedBall, rt')

    Complete -> (f, False, rt >>= (\x -> Just (x + frameScore f)))

processRoll :: [Frame] -> Int -> [Frame]
processRoll fs roll =
    reverse $ (\(a, _, _) -> a) $ foldl op ([], False, Just 0) fs
    where
        op (fs', bail, rt) f =
            if bail then
                (f : fs', bail, rt)
            else
                (f' : fs', bail', rt')
            where
                (f', bail', rt') = applyRollToFrame f roll rt

processRolls :: [Int] -> [Frame]
processRolls rolls =
    let
        fs = [Frame fn ReadyForFirstRoll Nothing Nothing Nothing Nothing [] | fn <- [1..maxFrames]]
    in
        foldl (\fs' roll -> processRoll fs' roll) fs rolls
