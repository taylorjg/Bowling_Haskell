import Test.QuickCheck
import Test.QuickCheck.Gen
import System.Random
import Bowling
import Control.Monad (join)

prop_FrameIntegrityAfterOneRoll :: [Rolls] -> Bool
prop_FrameIntegrityAfterOneRoll rss =
    runningTotal f1 == Nothing &&
    firstRoll f1 == Just r1 &&
    secondRoll f1 == Nothing &&
    thirdRoll f1 == Nothing
    where
        r1 = rs !! 0
        rs = join rss
        frames = processRolls $ take 1 rs
        f1 = frames !! 0

prop_FrameIntegrityAfterTwoRolls :: [Rolls] -> Bool
prop_FrameIntegrityAfterTwoRolls rss =
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
        r1 = rs !! 0
        r2 = rs !! 1
        rs = join rss
        frames = processRolls $ take 2 rs
        f1 = frames !! 0

validFrameRolls = [ [r1, r2] | r1 <- [0..9], r2 <- [0..10], r1 + r2 <= 10] ++ [[10]]
validFrameRollsChoice = elements validFrameRolls
rollsFor11Frames = vectorOf 11 validFrameRollsChoice

main = do
    quickCheck (forAll rollsFor11Frames prop_FrameIntegrityAfterOneRoll)
    quickCheck (forAll rollsFor11Frames prop_FrameIntegrityAfterTwoRolls)
