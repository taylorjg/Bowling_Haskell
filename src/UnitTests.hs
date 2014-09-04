import Test.HUnit
import Bowling
import System.Exit (exitFailure)
import Data.Either (isLeft)

assertFrameValue :: (Eq a, Show a) => Frames -> Int -> a -> (Frame -> a) -> String -> Assertion
assertFrameValue fs fn expected actualFn msg = do
    let f = fs !! (fn - 1)
    let msgSuffix = " in frame number " ++ show fn
    assertEqual (msg ++ msgSuffix) expected (actualFn f)

assertFrameNumber :: Frames -> Int -> Assertion
assertFrameNumber fs fn =
    assertFrameValue fs fn fn frameNumber "wrong frameNumber"

assertRunningTotal :: Frames -> Int -> Maybe RunningTotal -> Assertion
assertRunningTotal fs fn rt =
    assertFrameValue fs fn rt runningTotal "wrong runningTotal"

assertFirstRoll :: Frames -> Int -> Maybe Roll -> Assertion
assertFirstRoll fs fn r =
    assertFrameValue fs fn r firstRoll "wrong firstRoll"

assertSecondRoll :: Frames -> Int -> Maybe Roll -> Assertion
assertSecondRoll fs fn r =
    assertFrameValue fs fn r secondRoll "wrong secondRoll"

assertThirdRoll :: Frames -> Int -> Maybe Roll -> Assertion
assertThirdRoll fs fn r =
    assertFrameValue fs fn r thirdRoll "wrong thirdRoll"

testEmptyListOfRolls = TestCase $ do
    assertFrameNumber frames 1
    assertRunningTotal frames 1 Nothing
    assertFirstRoll frames 1 Nothing
    assertSecondRoll frames 1 Nothing
    assertThirdRoll frames 1 Nothing
    where
        Right frames = processRolls []

testSingleRoll = TestCase $ do
    assertFrameNumber frames 1
    assertRunningTotal frames 1 Nothing
    assertFirstRoll frames 1 (Just 4)
    assertSecondRoll frames 1 Nothing
    assertThirdRoll frames 1 Nothing
    where
        Right frames = processRolls [4]

testUninterestingFirstFrame = TestCase $ do
    assertFrameNumber frames 1
    assertRunningTotal frames 1 (Just 9)
    assertFirstRoll frames 1 (Just 4)
    assertSecondRoll frames 1 (Just 5)
    assertThirdRoll frames 1 Nothing
    where
        Right frames = processRolls [4, 5]

testFirstFrameSpareWithoutBonusBall = TestCase $ do
    assertFrameNumber frames 1
    assertRunningTotal frames 1 Nothing
    assertFirstRoll frames 1 (Just 4)
    assertSecondRoll frames 1 (Just 6)
    assertThirdRoll frames 1 Nothing
    where
        Right frames = processRolls [4, 6]

testFirstFrameSpareWithBonusBall = TestCase $ do
    assertFrameNumber frames 1
    assertRunningTotal frames 1 (Just 15)
    assertFirstRoll frames 1 (Just 4)
    assertSecondRoll frames 1 (Just 6)
    assertThirdRoll frames 1 Nothing
    where
        Right frames = processRolls [4, 6, 5]

testFirstFrameStrikeWithoutBonusBalls = TestCase $ do
    assertFrameNumber frames 1
    assertRunningTotal frames 1 Nothing
    assertFirstRoll frames 1 (Just 10)
    assertSecondRoll frames 1 Nothing
    assertThirdRoll frames 1 Nothing
    where
        Right frames = processRolls [10]

testFirstFrameStrikeWithFirstBonusBall = TestCase $ do
    assertFrameNumber frames 1
    assertRunningTotal frames 1 Nothing
    assertFirstRoll frames 1 (Just 10)
    assertSecondRoll frames 1 Nothing
    assertThirdRoll frames 1 Nothing
    where
        Right frames = processRolls [10, 4]

testFirstFrameStrikeWithBothBonusBalls = TestCase $ do
    assertFrameNumber frames 1
    assertRunningTotal frames 1 (Just 16)
    assertFirstRoll frames 1 (Just 10)
    assertSecondRoll frames 1 Nothing
    assertThirdRoll frames 1 Nothing
    where
        Right frames = processRolls [10, 4, 2]

testUninterestingLastFrame = TestCase $ do
    assertFrameNumber frames 10
    assertRunningTotal frames 10 (Just 9)
    assertFirstRoll frames 10 (Just 4)
    assertSecondRoll frames 10 (Just 5)
    assertThirdRoll frames 10 Nothing
    where
        Right frames = processRolls (replicate 18 0 ++ [4, 5])

testLastFrameSpareWithoutBonusBall = TestCase $ do
    assertFrameNumber frames 10
    assertRunningTotal frames 10 Nothing
    assertFirstRoll frames 10 (Just 8)
    assertSecondRoll frames 10 (Just 2)
    assertThirdRoll frames 10 Nothing
    where
        Right frames = processRolls (replicate 18 0 ++ [8, 2])

testLastFrameSpareWithBonusBall = TestCase $ do
    assertFrameNumber frames 10
    assertRunningTotal frames 10 (Just 15)
    assertFirstRoll frames 10 (Just 8)
    assertSecondRoll frames 10 (Just 2)
    assertThirdRoll frames 10 (Just 5)
    where
        Right frames = processRolls (replicate 18 0 ++ [8, 2, 5])

testLastFrameStrikeWithoutBonusBalls = TestCase $ do
    assertFrameNumber frames 10
    assertRunningTotal frames 10 Nothing
    assertFirstRoll frames 10 (Just 10)
    assertSecondRoll frames 10 Nothing
    assertThirdRoll frames 10 Nothing
    where
        Right frames = processRolls (replicate 18 0 ++ [10])

testLastFrameStrikeWithFirstBonusBall = TestCase $ do
    assertFrameNumber frames 10
    assertRunningTotal frames 10 Nothing
    assertFirstRoll frames 10 (Just 10)
    assertSecondRoll frames 10 (Just 4)
    assertThirdRoll frames 10 Nothing
    where
        Right frames = processRolls (replicate 18 0 ++ [10, 4])

testLastFrameStrikeWithBothBonusBalls = TestCase $ do
    assertFrameNumber frames 10
    assertRunningTotal frames 10 (Just 16)
    assertFirstRoll frames 10 (Just 10)
    assertSecondRoll frames 10 (Just 4)
    assertThirdRoll frames 10 (Just 2)
    where
        Right frames = processRolls (replicate 18 0 ++ [10, 4, 2])

testFrame1RollLessThanZeroResultsInAnError = TestCase $ do
    assertEqual "expected bowling result to indicate that an error occurred" True (isLeft br)
    where
        br = processRolls [-1]

testFrame1RollGreaterThanTenResultsInAnError = TestCase $ do
    assertEqual "expected bowling result to indicate that an error occurred" True (isLeft br)
    where
        br = processRolls [11]

testFrame3RollLessThanZeroResultsInAnError = TestCase $ do
    assertEqual "expected bowling result to indicate that an error occurred" True (isLeft br)
    where
        br = processRolls [1, 1, 2, 2, -1]

testFrame3RollGreaterThanTenResultsInAnError = TestCase $ do
    assertEqual "expected bowling result to indicate that an error occurred" True (isLeft br)
    where
        br = processRolls [1, 1, 2, 2, 11]

testFrame1RollTotalGreaterThanTenResultsInAnError = TestCase $ do
    assertEqual "expected bowling result to indicate that an error occurred" True (isLeft br)
    where
        br = processRolls [8, 3]

testFrame3RollTotalGreaterThanTenResultsInAnError = TestCase $ do
    assertEqual "expected bowling result to indicate that an error occurred" True (isLeft br)
    where
        br = processRolls [1, 1, 2, 2, 8, 3]

testRemainingRollAfterUninterestingLastFrameResultsInAnError = TestCase $ do
    assertEqual "expected bowling result to indicate that an error occurred" True (isLeft br)
    where
        br = processRolls (replicate 21 1)

testRemainingRollAfterLastFrameSpareResultsInAnError = TestCase $ do
    assertEqual "expected bowling result to indicate that an error occurred" True (isLeft br)
    where
        br = processRolls (replicate 18 0 ++ [8, 2, 0, 0])

testRemainingRollAfterLastFrameStrikeResultsInAnError = TestCase $ do
    assertEqual "expected bowling result to indicate that an error occurred" True (isLeft br)
    where
        br = processRolls (replicate 18 0 ++ [10, 1, 0, 0])

tests = TestList [
        TestLabel "testEmptyListOfRolls" testEmptyListOfRolls,
        TestLabel "testSingleRoll" testSingleRoll,
        TestLabel "testUninterestingFirstFrame" testUninterestingFirstFrame,
        TestLabel "testFirstFrameSpareWithoutBonusBall" testFirstFrameSpareWithoutBonusBall,
        TestLabel "testFirstFrameSpareWithBonusBall" testFirstFrameSpareWithBonusBall,
        TestLabel "testFirstFrameStrikeWithoutBonusBalls" testFirstFrameStrikeWithoutBonusBalls,
        TestLabel "testFirstFrameStrikeWithFirstBonusBall" testFirstFrameStrikeWithFirstBonusBall,
        TestLabel "testFirstFrameStrikeWithBothBonusBalls" testFirstFrameStrikeWithBothBonusBalls,
        TestLabel "testUninterestingLastFrame" testUninterestingLastFrame,
        TestLabel "testLastFrameSpareWithoutBonusBall" testLastFrameSpareWithoutBonusBall,
        TestLabel "testLastFrameSpareWithBonusBall" testLastFrameSpareWithBonusBall,
        TestLabel "testLastFrameStrikeWithoutBonusBalls" testLastFrameStrikeWithoutBonusBalls,
        TestLabel "testLastFrameStrikeWithFirstBonusBall" testLastFrameStrikeWithFirstBonusBall,
        TestLabel "testLastFrameStrikeWithBothBonusBalls" testLastFrameStrikeWithBothBonusBalls,
        TestLabel "testFrame1RollLessThanZeroResultsInAnError" testFrame1RollLessThanZeroResultsInAnError,
        TestLabel "testFrame1RollGreaterThanTenResultsInAnError" testFrame1RollGreaterThanTenResultsInAnError,
        TestLabel "testFrame3RollLessThanZeroResultsInAnError" testFrame3RollLessThanZeroResultsInAnError,
        TestLabel "testFrame3RollGreaterThanTenResultsInAnError" testFrame3RollGreaterThanTenResultsInAnError,
        TestLabel "testFrame1RollTotalGreaterThanTenResultsInAnError" testFrame1RollTotalGreaterThanTenResultsInAnError,
        TestLabel "testFrame3RollTotalGreaterThanTenResultsInAnError" testFrame3RollTotalGreaterThanTenResultsInAnError,
        TestLabel "testRemainingRollAfterUninterestingLastFrameResultsInAnError" testRemainingRollAfterUninterestingLastFrameResultsInAnError,
        TestLabel "testRemainingRollAfterLastFrameSpareResultsInAnError" testRemainingRollAfterLastFrameSpareResultsInAnError,
        TestLabel "testRemainingRollAfterLastFrameStrikeResultsInAnError" testRemainingRollAfterLastFrameStrikeResultsInAnError
    ]

main :: IO ()
main = do
    counts <- runTestTT tests
    if errors counts > 0 || failures counts > 0
        then exitFailure
        else return ()
