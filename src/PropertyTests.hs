import Test.QuickCheck
import Test.QuickCheck.Test
import System.Random
import Bowling
import Control.Monad (join)
import System.Exit (exitFailure)
import Data.Maybe

checkFrameInvariant :: Frame -> Bool
checkFrameInvariant f =
    isJust (runningTotal f) &&
    isJust (firstRoll f) &&
    if isLastFrame f then
        isJust (secondRoll f) &&
        if isStrikeFrame f then isJust (thirdRoll f) else True &&
        if isSpareFrame f then isJust (thirdRoll f) else True &&
        if not (isStrikeFrame f) && not (isSpareFrame f) then isNothing (thirdRoll f) else True
    else
        r1 + r2 <= maxPins &&
        if r1 == maxPins then isNothing (secondRoll f) else True &&
        isNothing (thirdRoll f)
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
    let numBonusBallsNeeded = calculateNumBonusBallsNeeded lastFrame
    return $ rolls ++ take numBonusBallsNeeded bonusBalls

calculateNumBonusBallsNeeded :: Rolls -> Int
calculateNumBonusBallsNeeded [maxPins] = 2
calculateNumBonusBallsNeeded [r1, r2]
    | r1 + r2 == maxPins = 1
    | otherwise = 0
    
main :: IO ()
main = do
    r1 <- quickCheckResult (forAll rollsGen prop_FrameInvariantHoldsForAllFrames)
    if all isSuccess [r1]
        then return ()
        else exitFailure
