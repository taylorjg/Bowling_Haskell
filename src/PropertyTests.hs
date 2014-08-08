import Test.QuickCheck
import Test.QuickCheck.Test
import Test.QuickCheck.Gen
import System.Random
import Bowling
import Control.Monad (join)
import System.Exit (exitFailure)

rollsFromRollsFor12Frames :: [Rolls] -> Rolls
rollsFromRollsFor12Frames rollsFor12Frames =
    rollsFor10Frames ++ take numLastFrameBonusBalls remainingRolls
    where
        rollsFor10Frames = join $ take 10 rollsFor12Frames
        remainingRolls = join $ drop 10 rollsFor12Frames
        lastFrame = rollsFor12Frames !! 9
        numLastFrameBonusBalls = calculateNumLastFrameBonusBalls lastFrame
        calculateNumLastFrameBonusBalls [maxPins] = 2
        calculateNumLastFrameBonusBalls [r1, r2]
            | r1 + r2 == maxPins = 1
            | otherwise = 0

prop_FrameIntegrityAfterOneRoll :: [Rolls] -> Bool
prop_FrameIntegrityAfterOneRoll rollsFor12Frames =
    runningTotal f1 == Nothing &&
    firstRoll f1 == Just r1 &&
    secondRoll f1 == Nothing &&
    thirdRoll f1 == Nothing
    where
        rs = rollsFromRollsFor12Frames rollsFor12Frames
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
        rs = rollsFromRollsFor12Frames rollsFor12Frames
        r1 = rs !! 0
        r2 = rs !! 1
        frames = processRolls $ take 2 rs
        f1 = frames !! 0

allValidFrameRolls = [ [r1, r2] | r1 <- [0..9], r2 <- [0..10], r1 + r2 <= 10] ++ [[10]]
allValidFrameRollsGen = elements allValidFrameRolls
rollsFor12FramesGen = vectorOf 12 allValidFrameRollsGen

main :: IO ()
main = do
    r1 <- quickCheckResult (forAll rollsFor12FramesGen prop_FrameIntegrityAfterOneRoll)
    r2 <- quickCheckResult (forAll rollsFor12FramesGen prop_FrameIntegrityAfterTwoRolls)
    if all isSuccess [r1, r2]
        then return ()
        else exitFailure
