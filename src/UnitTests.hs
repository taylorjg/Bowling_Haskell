import Test.HUnit
import Bowling
import System.Exit (exitFailure)

assertFrameValue :: (Eq a, Show a) => Frames -> Int -> a -> (Frame -> a) -> String -> Assertion
assertFrameValue fs fn expected actualFn msg = do
    let f = fs !! (fn - 1)
    let msgSuffix = " in frame number " ++ show fn
    let assertEqual' msg a b = assertEqual (msg ++ msgSuffix) a b
    assertEqual' msg expected (actualFn f)

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
        frames = processRolls []

testSingleRoll = TestCase $ do
    assertFrameNumber frames 1
    assertRunningTotal frames 1 Nothing
    assertFirstRoll frames 1 (Just 4)
    assertSecondRoll frames 1 Nothing
    assertThirdRoll frames 1 Nothing
    where
        frames = processRolls [4]

testUninterestingFirstFrame = TestCase $ do
    assertFrameNumber frames 1
    assertRunningTotal frames 1 (Just 9)
    assertFirstRoll frames 1 (Just 4)
    assertSecondRoll frames 1 (Just 5)
    assertThirdRoll frames 1 Nothing
    where
        frames = processRolls [4, 5]

testFirstFrameSpareWithoutBonusBall = TestCase $ do
    assertFrameNumber frames 1
    assertRunningTotal frames 1 Nothing
    assertFirstRoll frames 1 (Just 4)
    assertSecondRoll frames 1 (Just 6)
    assertThirdRoll frames 1 Nothing
    where
        frames = processRolls [4, 6]

testFirstFrameSpareWithBonusBall = TestCase $ do
    assertFrameNumber frames 1
    assertRunningTotal frames 1 (Just 15)
    assertFirstRoll frames 1 (Just 4)
    assertSecondRoll frames 1 (Just 6)
    assertThirdRoll frames 1 Nothing
    where
        frames = processRolls [4, 6, 5]

testFirstFrameStrikeWithoutBonusBalls = TestCase $ do
    assertFrameNumber frames 1
    assertRunningTotal frames 1 Nothing
    assertFirstRoll frames 1 (Just 10)
    assertSecondRoll frames 1 Nothing
    assertThirdRoll frames 1 Nothing
    where
        frames = processRolls [10]

testFirstFrameStrikeWithFirstBonusBall = TestCase $ do
    assertFrameNumber frames 1
    assertRunningTotal frames 1 Nothing
    assertFirstRoll frames 1 (Just 10)
    assertSecondRoll frames 1 Nothing
    assertThirdRoll frames 1 Nothing
    where
        frames = processRolls [10, 4]

testFirstFrameStrikeWithBothBonusBalls = TestCase $ do
    assertFrameNumber frames 1
    assertRunningTotal frames 1 (Just 16)
    assertFirstRoll frames 1 (Just 10)
    assertSecondRoll frames 1 Nothing
    assertThirdRoll frames 1 Nothing
    where
        frames = processRolls [10, 4, 2]

testUninterestingLastFrame = TestCase $ do
    assertFrameNumber frames 10
    assertRunningTotal frames 10 (Just 9)
    assertFirstRoll frames 10 (Just 4)
    assertSecondRoll frames 10 (Just 5)
    assertThirdRoll frames 10 Nothing
    where
        frames = processRolls (replicate 18 0 ++ [4, 5])

testLastFrameSpareWithoutBonusBall = TestCase $ do
    assertFrameNumber frames 10
    assertRunningTotal frames 10 Nothing
    assertFirstRoll frames 10 (Just 8)
    assertSecondRoll frames 10 (Just 2)
    assertThirdRoll frames 10 Nothing
    where
        frames = processRolls (replicate 18 0 ++ [8, 2])

testLastFrameSpareWithBonusBall = TestCase $ do
    assertFrameNumber frames 10
    assertRunningTotal frames 10 (Just 15)
    assertFirstRoll frames 10 (Just 8)
    assertSecondRoll frames 10 (Just 2)
    assertThirdRoll frames 10 (Just 5)
    where
        frames = processRolls (replicate 18 0 ++ [8, 2, 5])

testLastFrameStrikeWithoutBonusBalls = TestCase $ do
    assertFrameNumber frames 10
    assertRunningTotal frames 10 Nothing
    assertFirstRoll frames 10 (Just 10)
    assertSecondRoll frames 10 Nothing
    assertThirdRoll frames 10 Nothing
    where
        frames = processRolls (replicate 18 0 ++ [10])

testLastFrameStrikeWithFirstBonusBall = TestCase $ do
    assertFrameNumber frames 10
    assertRunningTotal frames 10 Nothing
    assertFirstRoll frames 10 (Just 10)
    assertSecondRoll frames 10 (Just 4)
    assertThirdRoll frames 10 Nothing
    where
        frames = processRolls (replicate 18 0 ++ [10, 4])

testLastFrameStrikeWithBothBonusBalls = TestCase $ do
    assertFrameNumber frames 10
    assertRunningTotal frames 10 (Just 16)
    assertFirstRoll frames 10 (Just 10)
    assertSecondRoll frames 10 (Just 4)
    assertThirdRoll frames 10 (Just 2)
    where
        frames = processRolls (replicate 18 0 ++ [10, 4, 2])

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
        TestLabel "testLastFrameStrikeWithBothBonusBalls" testLastFrameStrikeWithBothBonusBalls
    ]

main :: IO ()
main = do
    counts <- runTestTT tests
    if errors counts > 0 || failures counts > 0
        then exitFailure
        else return ()
