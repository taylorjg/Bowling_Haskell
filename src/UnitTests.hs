import Test.HUnit
import Bowling
import System.Exit (exitFailure)

assertFrameNumber :: Frames -> Int -> Assertion
assertFrameNumber frames fn = do
    let f = frames !! (fn - 1)
    let msgSuffix = " in frame number " ++ show fn
    let assertEqual' msg a b = assertEqual (msg ++ msgSuffix) a b
    assertEqual' "wrong frameNumber" fn (frameNumber f)

--assertFrameState :: Frames -> Int -> FrameState -> Assertion
--assertFrameState frames fn fs = do
--    let f = frames !! (fn - 1)
--    let msgSuffix = " in frame number " ++ show fn
--    let assertEqual' msg a b = assertEqual (msg ++ msgSuffix) a b
--    assertEqual' "wrong frameState" fs (frameState f)

assertRunningTotal :: Frames -> Int -> Maybe RunningTotal -> Assertion
assertRunningTotal frames fn rt = do
    let f = frames !! (fn - 1)
    let msgSuffix = " in frame number " ++ show fn
    let assertEqual' msg a b = assertEqual (msg ++ msgSuffix) a b
    assertEqual' "wrong runningTotal" rt (runningTotal f)

assertFirstRoll :: Frames -> Int -> Maybe Roll -> Assertion
assertFirstRoll frames fn r = do
    let f = frames !! (fn - 1)
    let msgSuffix = " in frame number " ++ show fn
    let assertEqual' msg a b = assertEqual (msg ++ msgSuffix) a b
    assertEqual' "wrong firstRoll" r (firstRoll f)

assertSecondRoll :: Frames -> Int -> Maybe Roll -> Assertion
assertSecondRoll frames fn r = do
    let f = frames !! (fn - 1)
    let msgSuffix = " in frame number " ++ show fn
    let assertEqual' msg a b = assertEqual (msg ++ msgSuffix) a b
    assertEqual' "wrong secondRoll" r (secondRoll f)

assertThirdRoll :: Frames -> Int -> Maybe Roll -> Assertion
assertThirdRoll frames fn r = do
    let f = frames !! (fn - 1)
    let msgSuffix = " in frame number " ++ show fn
    let assertEqual' msg a b = assertEqual (msg ++ msgSuffix) a b
    assertEqual' "wrong thirdRoll" r (thirdRoll f)

--assertNumBonusBallsNeeded :: Frames -> Int -> Int -> Assertion
--assertNumBonusBallsNeeded frames fn n = do
--    let f = frames !! (fn - 1)
--    let msgSuffix = " in frame number " ++ show fn
--    let assertEqual' msg a b = assertEqual (msg ++ msgSuffix) a b
--    assertEqual' "wrong numBonusBallsNeeded" n (numBonusBallsNeeded f)

--assertBonusBalls :: Frames -> Int -> Rolls -> Assertion
--assertBonusBalls frames fn rs = do
--    let f = frames !! (fn - 1)
--    let msgSuffix = " in frame number " ++ show fn
--    let assertEqual' msg a b = assertEqual (msg ++ msgSuffix) a b
--    assertEqual' "wrong bonusBalls" rs (bonusBalls f)

testEmptyListOfRolls = TestCase $ do
    assertFrameNumber frames 1
--    assertFrameState frames 1 ReadyForFirstRoll
    assertRunningTotal frames 1 Nothing
    assertFirstRoll frames 1 Nothing
    assertSecondRoll frames 1 Nothing
    assertThirdRoll frames 1 Nothing
--    assertNumBonusBallsNeeded frames 1 0
--    assertBonusBalls frames 1 []
    where
        frames = processRolls []

testSingleRoll = TestCase $ do
    assertFrameNumber frames 1
--    assertFrameState frames 1 ReadyForSecondRoll
    assertRunningTotal frames 1 Nothing
    assertFirstRoll frames 1 (Just 4)
    assertSecondRoll frames 1 Nothing
    assertThirdRoll frames 1 Nothing
--    assertNumBonusBallsNeeded frames 1 0
--    assertBonusBalls frames 1 []
    where
        frames = processRolls [4]

testUninterestingFirstFrame = TestCase $ do
    assertFrameNumber frames 1
--    assertFrameState frames 1 Complete
    assertRunningTotal frames 1 (Just 9)
    assertFirstRoll frames 1 (Just 4)
    assertSecondRoll frames 1 (Just 5)
    assertThirdRoll frames 1 Nothing
--    assertNumBonusBallsNeeded frames 1 0
--    assertBonusBalls frames 1 []
    where
        frames = processRolls [4, 5]

testFirstFrameSpareWithoutBonusBall = TestCase $ do
    assertFrameNumber frames 1
--    assertFrameState frames 1 NeedBonusBalls
    assertRunningTotal frames 1 Nothing
    assertFirstRoll frames 1 (Just 4)
    assertSecondRoll frames 1 (Just 6)
    assertThirdRoll frames 1 Nothing
--    assertNumBonusBallsNeeded frames 1 1
--    assertBonusBalls frames 1 []
    where
        frames = processRolls [4, 6]

testFirstFrameSpareWithBonusBall = TestCase $ do
    assertFrameNumber frames 1
--    assertFrameState frames 1 Complete
    assertRunningTotal frames 1 (Just 15)
    assertFirstRoll frames 1 (Just 4)
    assertSecondRoll frames 1 (Just 6)
    assertThirdRoll frames 1 Nothing
--    assertNumBonusBallsNeeded frames 1 0
--    assertBonusBalls frames 1 [5]
    where
        frames = processRolls [4, 6, 5]

testFirstFrameStrikeWithoutBonusBalls = TestCase $ do
    assertFrameNumber frames 1
--    assertFrameState frames 1 NeedBonusBalls
    assertRunningTotal frames 1 Nothing
    assertFirstRoll frames 1 (Just 10)
    assertSecondRoll frames 1 Nothing
    assertThirdRoll frames 1 Nothing
--    assertNumBonusBallsNeeded frames 1 2
--    assertBonusBalls frames 1 []
    where
        frames = processRolls [10]

testFirstFrameStrikeWithFirstBonusBall = TestCase $ do
    assertFrameNumber frames 1
--    assertFrameState frames 1 NeedBonusBalls
    assertRunningTotal frames 1 Nothing
    assertFirstRoll frames 1 (Just 10)
    assertSecondRoll frames 1 Nothing
    assertThirdRoll frames 1 Nothing
--    assertNumBonusBallsNeeded frames 1 1
--    assertBonusBalls frames 1 [4]
    where
        frames = processRolls [10, 4]

testFirstFrameStrikeWithBothBonusBalls = TestCase $ do
    assertFrameNumber frames 1
--    assertFrameState frames 1 Complete
    assertRunningTotal frames 1 (Just 16)
    assertFirstRoll frames 1 (Just 10)
    assertSecondRoll frames 1 Nothing
    assertThirdRoll frames 1 Nothing
--    assertNumBonusBallsNeeded frames 1 0
--    assertBonusBalls frames 1 [4, 2]
    where
        frames = processRolls [10, 4, 2]

testUninterestingLastFrame = TestCase $ do
    assertFrameNumber frames 10
--    assertFrameState frames 10 Complete
    assertRunningTotal frames 10 (Just 9)
    assertFirstRoll frames 10 (Just 4)
    assertSecondRoll frames 10 (Just 5)
    assertThirdRoll frames 1 Nothing
--    assertNumBonusBallsNeeded frames 10 0
--    assertBonusBalls frames 10 []
    where
        frames = processRolls (replicate 18 0 ++ [4, 5])

testLastFrameSpareWithoutBonusBall = TestCase $ do
    assertFrameNumber frames 10
--    assertFrameState frames 10 NeedBonusBalls
    assertRunningTotal frames 10 Nothing
    assertFirstRoll frames 10 (Just 8)
    assertSecondRoll frames 10 (Just 2)
    assertThirdRoll frames 1 Nothing
--    assertNumBonusBallsNeeded frames 10 1
--    assertBonusBalls frames 10 []
    where
        frames = processRolls (replicate 18 0 ++ [8, 2])

testLastFrameSpareWithBonusBall = TestCase $ do
    assertFrameNumber frames 10
--    assertFrameState frames 10 Complete
    assertRunningTotal frames 10 (Just 15)
    assertFirstRoll frames 10 (Just 8)
    assertSecondRoll frames 10 (Just 2)
    assertThirdRoll frames 1 (Just 5)
--    assertNumBonusBallsNeeded frames 10 0
--    assertBonusBalls frames 10 [5]
    where
        frames = processRolls (replicate 18 0 ++ [8, 2, 5])

testLastFrameStrikeWithoutBonusBalls = TestCase $ do
    assertFrameNumber frames 10
--    assertFrameState frames 10 NeedBonusBalls
    assertRunningTotal frames 10 Nothing
    assertFirstRoll frames 10 (Just 10)
    assertSecondRoll frames 10 Nothing
    assertThirdRoll frames 1 Nothing
--    assertNumBonusBallsNeeded frames 10 2
--    assertBonusBalls frames 10 []
    where
        frames = processRolls (replicate 18 0 ++ [10])

testLastFrameStrikeWithFirstBonusBall = TestCase $ do
    assertFrameNumber frames 10
--    assertFrameState frames 10 NeedBonusBalls
    assertRunningTotal frames 10 Nothing
    assertFirstRoll frames 10 (Just 10)
    assertSecondRoll frames 10 (Just 4)
    assertThirdRoll frames 1 Nothing
--    assertNumBonusBallsNeeded frames 10 1
--    assertBonusBalls frames 10 [4]
    where
        frames = processRolls (replicate 18 0 ++ [10, 4])

testLastFrameStrikeWithBothBonusBalls = TestCase $ do
    assertFrameNumber frames 10
--    assertFrameState frames 10 Complete
    assertRunningTotal frames 10 (Just 16)
    assertFirstRoll frames 10 (Just 10)
    assertSecondRoll frames 10 (Just 4)
    assertThirdRoll frames 1 (Just 2)
--    assertNumBonusBallsNeeded frames 10 0
--    assertBonusBalls frames 10 [4, 2]
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
