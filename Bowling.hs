import Data.Maybe
import Data.List

data FrameState
	= ReadyForFirstRoll
	| ReadyForSecondRoll
	| SpareNeedOneMore
	| StrikeNeedTwoMore
	| StrikeNeedOneMore
	| Complete
	deriving (Eq, Show)

data Roll = Roll {
	value :: Int,
	isSpare :: Bool}
	deriving Show

isStrike :: Roll -> Bool
isStrike roll = value roll == 10

data Frame = Frame {
	frameNumber :: Int,
	frameState :: FrameState,
	score :: Int,
	firstRoll :: Maybe Roll,
	secondRoll :: Maybe Roll,
	thirdRoll :: Maybe Roll}
	deriving Show

isLastFrame :: Frame -> Bool
isLastFrame f = frameNumber f == 10

isFrameComplete :: Frame -> Bool
isFrameComplete f = frameState f == Complete

applyRollToFrame :: Frame -> Int -> (Frame, Bool)
applyRollToFrame f roll = case frameState f of

	ReadyForFirstRoll ->
		let
			newRoll = Roll roll False
			newFrameState = if isStrike newRoll then StrikeNeedTwoMore else ReadyForSecondRoll
			newScore = score f + roll
			newFrame = f { 
				frameState = newFrameState, 
				score = newScore,
				firstRoll = Just newRoll}
		in
			(newFrame, True)

	ReadyForSecondRoll ->
		let
			isSpare = (value $ fromJust $ firstRoll f) + roll == 10
			newRoll = Roll roll isSpare
			newScore = score f + roll
			newFrameState = if isSpare then SpareNeedOneMore else Complete
			newFrame = f { 
				frameState = newFrameState, 
				score = newScore,
				secondRoll = Just newRoll}
		in
			(newFrame, True)

	SpareNeedOneMore ->
		let
			newRoll = if isLastFrame f then Just (Roll roll False) else Nothing
			newScore = score f + roll
			newFrameState = Complete
			newFrame = f { 
				frameState = newFrameState, 
				score = newScore,
				thirdRoll = newRoll}
		in
			(newFrame, False)

	StrikeNeedTwoMore ->
		let
			newRoll = if isLastFrame f then Just (Roll roll False) else Nothing
			newScore = score f + roll
			newFrameState = StrikeNeedOneMore
			newFrame = f { 
				frameState = newFrameState, 
				score = newScore,
				secondRoll = newRoll}
		in
			(newFrame, False)

	StrikeNeedOneMore ->
		let
			newRoll = if isLastFrame f then Just (Roll roll False) else Nothing
			newScore = score f + roll
			newFrameState = Complete
			newFrame = f { 
				frameState = newFrameState, 
				score = newScore,
				secondRoll = newRoll}
		in
			(newFrame, False)

	Complete -> (f, False)


processRoll :: [Frame] -> Int -> [Frame]
processRoll fs roll =
	reverse $ fst $ foldl doIt ([], False) fs
	where
		doIt (fs', bail) f =
			if bail then
				(f : fs', True)
			else
				(f' : fs', bail')
			where
				(f', bail') = applyRollToFrame f roll

processRolls :: [Int] -> [Frame]
processRolls rolls =
	let
		fs = [Frame fn ReadyForFirstRoll 0 Nothing Nothing Nothing | fn <- [1..10]]
	in
		foldl (\fs' r -> processRoll fs' r) fs rolls

