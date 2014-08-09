import Test.QuickCheck
import Test.QuickCheck.Test
import Test.QuickCheck.Gen
import System.Random
import Bowling
import Control.Monad (join)
import System.Exit (exitFailure)

prop_FrameIntegrityAfterOneRoll :: Rolls -> Bool
prop_FrameIntegrityAfterOneRoll rolls =
    runningTotal f1 == Nothing &&
    firstRoll f1 == Just r1 &&
    secondRoll f1 == Nothing &&
    thirdRoll f1 == Nothing
    where
        frames = processRolls $ take 1 rolls
        f1 = frames !! 0
        r1 = rolls !! 0

prop_FrameIntegrityAfterTwoRolls :: Rolls -> Bool
prop_FrameIntegrityAfterTwoRolls rolls =
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
        frames = processRolls $ take 2 rolls
        f1 = frames !! 0
        r1 = rolls !! 0
        r2 = rolls !! 1

-- TODO: use 'frequency' to get more strikes e.g. 50% of all pairs
pairsOfRolls = [ [r1, r2] | r1 <- [0..9], r2 <- [0..10], r1 + r2 <= 10] ++ [[10]] :: [Rolls]
pairsOfRollsGen = elements pairsOfRolls

rollsGen :: Gen Rolls
rollsGen = do
    tenFrames <- vectorOf 10 pairsOfRollsGen
    twoFrames <- vectorOf 2 pairsOfRollsGen
    let lastFrame = last tenFrames
    let rolls = join tenFrames
    let bonusBalls = join twoFrames
    let numBonusBalls = calculateNumBonusBalls lastFrame
    return $ rolls ++ take numBonusBalls bonusBalls

calculateNumBonusBalls :: Rolls -> Int
calculateNumBonusBalls [maxPins] = 2
calculateNumBonusBalls [r1, r2]
    | r1 + r2 == maxPins = 1
    | otherwise = 0
    
main :: IO ()
main = do
    sample rollsGen
    r1 <- quickCheckResult (forAll rollsGen prop_FrameIntegrityAfterOneRoll)
    r2 <- quickCheckResult (forAll rollsGen prop_FrameIntegrityAfterTwoRolls)
    if all isSuccess [r1, r2]
        then return ()
        else exitFailure
