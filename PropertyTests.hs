import Test.QuickCheck
import Test.QuickCheck.Gen
import System.Random
import Bowling
import Control.Monad (join)

rollsFromRollsFor12rames :: [Rolls] -> Rolls
rollsFromRollsFor12rames rollsFor12Frames =
    rollsFor10Frames ++ take numLastFrameBonusBalls remainingRolls
    where
        rollsFor10Frames = join $ take 10 rollsFor12Frames
        remainingRolls = join $ drop 10 rollsFor12Frames
        f10 = rollsFor12Frames !! 9
        f10r1 = head f10
        f10sum = sum f10
        numLastFrameBonusBalls = if f10r1 == maxPins
            then 2
            else if f10sum == maxPins
                then 1
                else 0

prop_FrameIntegrityAfterOneRoll :: [Rolls] -> Bool
prop_FrameIntegrityAfterOneRoll rollsFor12Frames =
    runningTotal f1 == Nothing &&
    firstRoll f1 == Just r1 &&
    secondRoll f1 == Nothing &&
    thirdRoll f1 == Nothing
    where
        rs = rollsFromRollsFor12rames rollsFor12Frames
        r1 = rs !! 0
        frames = processRolls $ take 1 rs
        f1 = frames !! 0

prop_FrameIntegrityAfterTwoRolls :: [Rolls] -> Bool
prop_FrameIntegrityAfterTwoRolls rollsFor12Frames =
    if r1 == maxPins then
            runningTotal f1 == Nothing &&
            firstRoll f1 == Just r1 &&
            secondRoll f1 == Nothing &&
            thirdRoll f1 == Nothing
    else
        runningTotal f1 == (if r1 + r2 == maxPins then Nothing else Just (r1 + r2)) &&
        firstRoll f1 == Just r1 &&
        secondRoll f1 == Just r2 &&
        thirdRoll f1 == Nothing
    where
        rs = rollsFromRollsFor12rames rollsFor12Frames
        r1 = rs !! 0
        r2 = rs !! 1
        frames = processRolls $ take 2 rs
        f1 = frames !! 0

allValidFrameRolls = [ [r1, r2] | r1 <- [0..9], r2 <- [0..10], r1 + r2 <= 10] ++ [[10]]
allValidFrameRollsGen = elements allValidFrameRolls
rollsFor12FramesGen = vectorOf 12 allValidFrameRollsGen

main = do
    quickCheck (forAll rollsFor12FramesGen prop_FrameIntegrityAfterOneRoll)
    quickCheck (forAll rollsFor12FramesGen prop_FrameIntegrityAfterTwoRolls)
