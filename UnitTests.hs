import Test.HUnit
import Bowling
import System.IO

assertFrame :: Frames -> Int -> Frame -> Assertion
assertFrame frames fn f2 = do
    let f1 = frames !! (fn - 1)
    let msgSuffix = " in frame number " ++ show fn
    let assertEqual' msg a b = assertEqual (msg ++ msgSuffix) a b
    assertEqual' "wrong frameNumber" (frameNumber f2) (frameNumber f1)
    assertEqual' "wrong runningTotal" (runningTotal f2) (runningTotal f1)
    assertEqual' "wrong firstRoll" (firstRoll f2) (firstRoll f1)
    assertEqual' "wrong secondRoll" (secondRoll f2) (secondRoll f1)
    assertEqual' "wrong thirdRoll" (thirdRoll f2) (thirdRoll f1)
    assertEqual' "wrong bonusBalls" (bonusBalls f2) (bonusBalls f1)

testEmptyListOfRolls = TestCase $ do
    assertFrame frames 1 expectedFrame
    where
        frames = processRolls []
        expectedFrame = frameDefault { frameNumber = 1 }

testSingleRoll = TestCase $ do
    assertFrame frames 1 expectedFrame
    where
        frames = processRolls [4]
        expectedFrame = frameDefault {
            frameNumber = 1,
            firstRoll = Just 4}

testUninterestingFirstFrame = TestCase $ do
    assertFrame frames 1 expectedFrame
    where
        frames = processRolls [4, 5]
        expectedFrame = frameDefault {
            frameNumber = 1,
            runningTotal = Just 9,
            firstRoll = Just 4,
            secondRoll = Just 5}

testFirstFrameSpareWithoutBonusBall = TestCase $ do
    assertFrame frames 1 expectedFrame
    where
        frames = processRolls [4, 6]
        expectedFrame = frameDefault {
            frameNumber = 1,
            firstRoll = Just 4,
            secondRoll = Just 6}

testFirstFrameSpareWithBonusBall = TestCase $ do
    assertFrame frames 1 expectedFrame
    where
        frames = processRolls [4, 6, 5]
        expectedFrame = frameDefault {
            frameNumber = 1,
            runningTotal = Just 15,
            firstRoll = Just 4,
            secondRoll = Just 6,
            bonusBalls = [5]}

testFirstFrameStrikeWithoutBonusBalls = TestCase $ do
    assertFrame frames 1 expectedFrame
    where
        frames = processRolls [10]
        expectedFrame = frameDefault {
            frameNumber = 1,
            firstRoll = Just 10}

testFirstFrameStrikeWithFirstBonusBall = TestCase $ do
    assertFrame frames 1 expectedFrame
    where
        frames = processRolls [10, 4]
        expectedFrame = frameDefault {
            frameNumber = 1,
            firstRoll = Just 10,
            bonusBalls = [4]}

testFirstFrameStrikeWithBothBonusBalls = TestCase $ do
    assertFrame frames 1 expectedFrame
    where
        frames = processRolls [10, 4, 2]
        expectedFrame = frameDefault {
            frameNumber = 1,
            runningTotal = Just 16,
            firstRoll = Just 10,
            bonusBalls = [4, 2]}

testUninterestingLastFrame = TestCase $ do
    assertFrame frames 10 expectedFrame
    where
        frames = processRolls (replicate 18 0 ++ [4, 5])
        expectedFrame = frameDefault {
            frameNumber = 10,
            runningTotal = Just 9,
            firstRoll = Just 4,
            secondRoll = Just 5}

testLastFrameSpareWithoutBonusBall = TestCase $ do
    assertFrame frames 10 expectedFrame
    where
        frames = processRolls (replicate 18 0 ++ [8, 2])
        expectedFrame = frameDefault {
            frameNumber = 10,
            firstRoll = Just 8,
            secondRoll = Just 2}

testLastFrameSpareWithBonusBall = TestCase $ do
    assertFrame frames 10 expectedFrame
    where
        frames = processRolls (replicate 18 0 ++ [8, 2, 5])
        expectedFrame = frameDefault {
            frameNumber = 10,
            runningTotal = Just 15,
            firstRoll = Just 8,
            secondRoll = Just 2,
            thirdRoll = Just 5}

testLastFrameStrikeWithoutBonusBalls = TestCase $ do
    assertFrame frames 10 expectedFrame
    where
        frames = processRolls (replicate 18 0 ++ [10])
        expectedFrame = frameDefault {
            frameNumber = 10,
            firstRoll = Just 10}

testLastFrameStrikeWithFirstBonusBall = TestCase $ do
    assertFrame frames 10 expectedFrame
    where
        frames = processRolls (replicate 18 0 ++ [10, 4])
        expectedFrame = frameDefault {
            frameNumber = 10,
            firstRoll = Just 10,
            secondRoll = Just 4}

testLastFrameStrikeWithBothBonusBalls = TestCase $ do
    assertFrame frames 10 expectedFrame
    where
        frames = processRolls (replicate 18 0 ++ [10, 4, 2])
        expectedFrame = frameDefault {
            frameNumber = 10,
            runningTotal = Just 16,
            firstRoll = Just 10,
            secondRoll = Just 4,
            thirdRoll = Just 2}

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

main = runTestTT tests
