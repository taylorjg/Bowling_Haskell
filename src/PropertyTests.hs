import Test.QuickCheck
import Test.QuickCheck.Test
import System.Random
import Bowling
import Control.Monad (join)
import System.Exit (exitFailure)
import Data.Maybe

checkFrameInvariant :: Frame -> Bool
checkFrameInvariant f =
    frameState f == Complete &&
    isJust (runningTotal f) &&
    isJust (firstRoll f) &&
    numBonusBallsNeeded f == 0 &&
    r1 + r2 <= maxPins &&
    if r1 == maxPins then length (bonusBalls f) == 2 else True &&
    if r1 == maxPins then isNothing (secondRoll f) else True &&
    if r1 + r2 == maxPins then length (bonusBalls f) == 1 else True
    where
        r1 = fromMaybe 0 $ firstRoll f
        r2 = fromMaybe 0 $ secondRoll f

prop_FrameInvariantHoldsForAllFrames :: Rolls -> Bool
prop_FrameInvariantHoldsForAllFrames rolls =
    all checkFrameInvariant frames
    where
        frames = processRolls rolls

nonStrikeFrameRolls = [[r1, r2] | r1 <- [0..9], r2 <- [0..10], r1 + r2 <= 10]
nonStrikeFrameRollsGen = elements nonStrikeFrameRolls

strikeFrameRollsGen = return [10]

frameGen = frequency [(40, nonStrikeFrameRollsGen), (60, strikeFrameRollsGen)]

rollsGen :: Gen Rolls
rollsGen = do
    tenFrames <- vectorOf 10 frameGen
    twoFrames <- vectorOf 2 frameGen
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
    r1 <- quickCheckResult (forAll rollsGen prop_FrameInvariantHoldsForAllFrames)
    if all isSuccess [r1]
        then return ()
        else exitFailure
