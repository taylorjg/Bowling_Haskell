import Test.QuickCheck
import Test.QuickCheck.Test
import System.Random
import Bowling
import Control.Monad (join)
import System.Exit (exitFailure)
import Data.Maybe
import Data.Either (isLeft)

checkFrameInvariant :: Frame -> Bool
checkFrameInvariant f =
    isJust (runningTotal f) &&
    isJust (firstRoll f) &&
    if isLastFrame f then
        isJust (secondRoll f) &&
        if isStrikeFrame f || isSpareFrame f then isJust (thirdRoll f) else True &&
        if not (isStrikeFrame f) && not (isSpareFrame f) then isNothing (thirdRoll f) else True
    else
        r1 + r2 <= maxPins &&
        if isStrikeFrame f then isNothing (secondRoll f) else True &&
        isNothing (thirdRoll f)
    where
        r1 = fromMaybe 0 $ firstRoll f
        r2 = fromMaybe 0 $ secondRoll f

prop_FrameInvariantHoldsForAllFrames :: Rolls -> Bool
prop_FrameInvariantHoldsForAllFrames rolls =
    all checkFrameInvariant frames
    where
        Right frames = processRolls rolls

prop_VariousBadRollsResultInAnError :: Rolls -> Bool
prop_VariousBadRollsResultInAnError rolls =
    isLeft br
    where
        br = processRolls rolls

nonStrikeValidFrameRolls = [[r1, r2] | r1 <- [0..9], r2 <- [0..10], r1 + r2 <= 10]
nonStrikeValidFrameRollsGen = elements nonStrikeValidFrameRolls

strikeValidFrameRollsGen = return [10]

firstRollTooSmallFrameRolls = [[r1, r2] | r1 <- [(-10)..(-1)], r2 <- [0..10]]
firstRollTooSmallFrameRollsGen = elements firstRollTooSmallFrameRolls

firstRollTooBigFrameRolls = [[r1, r2] | r1 <- [11..20], r2 <- [0..10]]
firstRollTooBigFrameRollsGen = elements firstRollTooBigFrameRolls

secondRollTooSmallFrameRolls = [[r1, r2] | r1 <- [0..9], r2 <- [(-10)..(-1)]]
secondRollTooSmallFrameRollsGen = elements secondRollTooSmallFrameRolls

secondRollTooBigFrameRolls = [[r1, r2] | r1 <- [0..9], r2 <- [11..20]]
secondRollTooBigFrameRollsGen = elements secondRollTooBigFrameRolls

totalTooBigFrameRolls = [[r1, r2] | r1 <- [0..9], r2 <- [0..10], r1 + r2 > 10]
totalTooBigFrameRollsGen = elements totalTooBigFrameRolls

validFrameGen                   = frequency [(40, nonStrikeValidFrameRollsGen),     (60, strikeValidFrameRollsGen)]
someFirstRollsTooSmallFrameGen  = frequency [(70, firstRollTooSmallFrameRollsGen),  (15, nonStrikeValidFrameRollsGen), (15, strikeValidFrameRollsGen)]
someFirstRollsTooBigFrameGen    = frequency [(70, firstRollTooBigFrameRollsGen),    (15, nonStrikeValidFrameRollsGen), (15, strikeValidFrameRollsGen)]
someSecondRollsTooSmallFrameGen = frequency [(70, secondRollTooSmallFrameRollsGen), (15, nonStrikeValidFrameRollsGen), (15, strikeValidFrameRollsGen)]
someSecondRollsTooBigFrameGen   = frequency [(70, secondRollTooBigFrameRollsGen),   (15, nonStrikeValidFrameRollsGen), (15, strikeValidFrameRollsGen)]
someTotalTooBigFrameGen         = frequency [(70, totalTooBigFrameRollsGen),        (15, nonStrikeValidFrameRollsGen), (15, strikeValidFrameRollsGen)]

rollsGen :: Gen Rolls -> Int -> Gen Rolls
rollsGen fg numSuperfluousBalls = do
    tenFrames <- vectorOf 10 fg
    bonusBallFrames <- vectorOf 2 fg
    superfluousBallFrames <- vectorOf 2 fg
    let lastFrame = last tenFrames
    let rollsForTenFrames = join tenFrames
    let bonusBalls = join bonusBallFrames
    let superfluousBalls = join superfluousBallFrames
    let numBonusBallsNeeded = calculateNumBonusBallsNeeded lastFrame
    return $
        rollsForTenFrames ++
        take numBonusBallsNeeded bonusBalls ++
        take numSuperfluousBalls superfluousBalls

calculateNumBonusBallsNeeded :: Rolls -> Int
calculateNumBonusBallsNeeded [r1]
    | r1 == maxPins = 2
    | otherwise = error "a frame consisting of a single roll must be a strike!"
calculateNumBonusBallsNeeded [r1, r2]
    | r1 + r2 == maxPins = 1
    | otherwise = 0

main :: IO ()
main = do
    r1 <- quickCheckResult (forAll (rollsGen validFrameGen 0) prop_FrameInvariantHoldsForAllFrames)
    r2 <- quickCheckResult (forAll (rollsGen someFirstRollsTooSmallFrameGen 0) prop_VariousBadRollsResultInAnError)
    r3 <- quickCheckResult (forAll (rollsGen someFirstRollsTooBigFrameGen 0) prop_VariousBadRollsResultInAnError)
    r4 <- quickCheckResult (forAll (rollsGen someSecondRollsTooSmallFrameGen 0) prop_VariousBadRollsResultInAnError)
    r5 <- quickCheckResult (forAll (rollsGen someSecondRollsTooBigFrameGen 0) prop_VariousBadRollsResultInAnError)
    r6 <- quickCheckResult (forAll (rollsGen someTotalTooBigFrameGen 0) prop_VariousBadRollsResultInAnError)
    r7 <- quickCheckResult (forAll (rollsGen validFrameGen 1) prop_VariousBadRollsResultInAnError)
    if all isSuccess [r1, r2, r3, r4, r5, r6, r7]
        then return ()
        else exitFailure
