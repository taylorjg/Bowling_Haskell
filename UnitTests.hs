import Test.HUnit
import Bowling
import System.IO

assertFrame :: Frames -> Int -> Frame -> Assertion
assertFrame frames n f2 = do
    let f1 = frames !! n
    let msgSuffix = " in frame number " ++ show (n + 1)
    let localAssertEqual msg a b = assertEqual (msg ++ msgSuffix) a b
    localAssertEqual "wrong frameNumber" (frameNumber f1) (frameNumber f2)
    localAssertEqual "wrong runningTotal" (runningTotal f1) (runningTotal f2)
    localAssertEqual "wrong firstRoll" (firstRoll f1) (firstRoll f2)
    localAssertEqual "wrong secondRoll" (secondRoll f1) (secondRoll f2)
    localAssertEqual "wrong thirdRoll" (thirdRoll f1) (thirdRoll f2)
    localAssertEqual "wrong bonusBalls" (bonusBalls f1) (bonusBalls f2)

testEmptyListOfRolls = TestCase $ do
    assertFrame frames 0 expectedFrame
    where
        frames = processRolls []
        expectedFrame = frameDefault { frameNumber = 1 }

testSingleRoll = TestCase $ do
    assertFrame frames 0 expectedFrame
    where
        frames = processRolls [4]
        expectedFrame = frameDefault {
            frameNumber = 1,
            firstRoll = Just 4}

testUninterestingFirstFrame = TestCase $ do
    assertFrame frames 0 expectedFrame
    where
        frames = processRolls [4, 5]
        expectedFrame = frameDefault {
            frameNumber = 1,
            runningTotal = Just 9,
            firstRoll = Just 4,
            secondRoll = Just 5}

testFirstFrameSpareWithoutBonusBall = TestCase $ do
    assertFrame frames 0 expectedFrame
    where
        frames = processRolls [4, 6]
        expectedFrame = frameDefault {
            frameNumber = 1,
            firstRoll = Just 4,
            secondRoll = Just 6}

testFirstFrameSpareWithBonusBall = TestCase $ do
    assertFrame frames 0 expectedFrame
    where
        frames = processRolls [4, 6, 5]
        expectedFrame = frameDefault {
            frameNumber = 1,
            runningTotal = Just 15,
            firstRoll = Just 4,
            secondRoll = Just 6,
            bonusBalls = [5]}

tests = TestList [
        testEmptyListOfRolls,
        testSingleRoll,
        testUninterestingFirstFrame,
        testFirstFrameSpareWithoutBonusBall,
        testFirstFrameSpareWithBonusBall
    ]

main = runTestTT tests
