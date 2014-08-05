import Test.HUnit
import Bowling
import System.IO

test1 = TestCase $ do
	let frames = processRolls []	
	let f1 = frames !! 0
	assertEqual "frameNumber of frame 1" 1 (frameNumber f1)
	assertEqual "runningTotal of frame 1" Nothing (runningTotal f1)
	assertEqual "firstRoll of frame 1" Nothing (firstRoll f1)
	assertEqual "secondRoll of frame 1" Nothing (secondRoll f1)
	assertEqual "thirdRoll of frame 1" Nothing (thirdRoll f1)
	assertEqual "bonusBalls of frame 1" [] (bonusBalls f1)

test2 = TestCase $ do
	let frames = processRolls [4]
	let f1 = frames !! 0
	assertEqual "frameNumber of frame 1" 1 (frameNumber f1)
	assertEqual "runningTotal of frame 1" Nothing (runningTotal f1)
	assertEqual "firstRoll of frame 1" (Just 4) (firstRoll f1)
	assertEqual "secondRoll of frame 1" Nothing (secondRoll f1)
	assertEqual "thirdRoll of frame 1" Nothing (thirdRoll f1)
	assertEqual "bonusBalls of frame 1" [] (bonusBalls f1)

test3 = TestCase $ do
	let frames = processRolls [4, 5]
	let f1 = frames !! 0
	assertEqual "frameNumber of frame 1" 1 (frameNumber f1)
	assertEqual "runningTotal of frame 1" (Just 9) (runningTotal f1)
	assertEqual "firstRoll of frame 1" (Just 4) (firstRoll f1)
	assertEqual "secondRoll of frame 1" (Just 5) (secondRoll f1)
	assertEqual "thirdRoll of frame 1" Nothing (thirdRoll f1)
	assertEqual "bonusBalls of frame 1" [] (bonusBalls f1)

test4 = TestCase $ do
	let frames = processRolls [4, 6]
	let f1 = frames !! 0
	assertEqual "frameNumber of frame 1" 1 (frameNumber f1)
	assertEqual "runningTotal of frame 1" Nothing (runningTotal f1)
	assertEqual "firstRoll of frame 1" (Just 4) (firstRoll f1)
	assertEqual "secondRoll of frame 1" (Just 6) (secondRoll f1)
	assertEqual "thirdRoll of frame 1" Nothing (thirdRoll f1)
	assertEqual "bonusBalls of frame 1" [] (bonusBalls f1)

test5 = TestCase $ do
	let frames = processRolls [4, 6, 5]
	let f1 = frames !! 0
	assertEqual "frameNumber of frame 1" 1 (frameNumber f1)
	assertEqual "runningTotal of frame 1" (Just 15) (runningTotal f1)
	assertEqual "firstRoll of frame 1" (Just 4) (firstRoll f1)
	assertEqual "secondRoll of frame 1" (Just 6) (secondRoll f1)
	assertEqual "thirdRoll of frame 1" Nothing (thirdRoll f1)
	assertEqual "bonusBalls of frame 1" [5] (bonusBalls f1)

tests = TestList [
	TestLabel "empty list of rolls" test1,
	TestLabel "1 roll" test2,
	TestLabel "2 rolls adding up to less than 10" test3,
	TestLabel "2 rolls => spare => first frame is incomplete" test4,
	TestLabel "3 rolls => first frame is complete" test5
	]

main = runTestText (putTextToHandle stderr True) tests
