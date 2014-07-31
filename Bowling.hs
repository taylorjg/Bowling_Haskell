import Data.List
import Data.Maybe

maxPins = 10
maxFrames = 10

data FrameState
	= ReadyForFirstRoll
	| ReadyForSecondRoll
	| SpareNeedOneMore
	| StrikeNeedTwoMore
	| StrikeNeedOneMore
	| Complete
	deriving (Eq, Show)

data Frame = Frame {
	frameNumber :: Int,
	frameState :: FrameState,
	score :: Int,
	runningTotal :: Maybe Int,
	firstRoll :: Maybe Int,
	secondRoll :: Maybe Int,
	thirdRoll :: Maybe Int}
	deriving Show

isStrike :: Int -> Bool
isStrike roll = roll == maxPins

isLastFrame :: Frame -> Bool
isLastFrame f = frameNumber f == maxFrames

applyRollToFrame :: Frame -> Int -> Maybe Int -> (Frame, Bool, Maybe Int)
applyRollToFrame f roll rt = case frameState f of

	ReadyForFirstRoll ->
		let
			newFrameState = if isStrike roll then StrikeNeedTwoMore else ReadyForSecondRoll
			newScore = score f + roll
			newFrame = f { 
				frameState = newFrameState, 
				score = newScore,
				firstRoll = Just roll}
		in
			(newFrame, True, Nothing)

	ReadyForSecondRoll ->
		let
			newScore = score f + roll
			isSpare = (fromJust $ firstRoll f) + roll == maxPins
			newFrameState = if isSpare then SpareNeedOneMore else Complete
			rt' = if newFrameState == Complete then
					case rt of
						Nothing -> Nothing
						Just x -> Just (x + newScore)
				else
					Nothing
			newFrame = f { 
				frameState = newFrameState, 
				score = newScore,
				runningTotal = rt',
				secondRoll = Just roll}
		in
			(newFrame, True, rt')

	SpareNeedOneMore ->
		let
			thirdRoll = if isLastFrame f then Just roll else Nothing
			newScore = score f + roll
			newFrameState = Complete
			rt' = case rt of
				Nothing -> Nothing
				Just x -> Just (x + newScore)
			newFrame = f { 
				frameState = newFrameState, 
				score = newScore,
				runningTotal = rt',
				thirdRoll = thirdRoll}
		in
			-- BUG: technically, if this is the last frame, we have consumed this roll.
			(newFrame, False, rt')

	StrikeNeedTwoMore ->
		let
			secondRoll = if isLastFrame f then Just roll else Nothing
			newScore = score f + roll
			newFrameState = StrikeNeedOneMore
			newFrame = f { 
				frameState = newFrameState, 
				score = newScore,
				secondRoll = secondRoll}
		in
			(newFrame, False, Nothing)

	StrikeNeedOneMore ->
		let
			thirdRoll = if isLastFrame f then Just roll else Nothing
			newScore = score f + roll
			newFrameState = Complete
			rt' = case rt of
				Nothing -> Nothing
				Just x -> Just (x + newScore)
			newFrame = f { 
				frameState = newFrameState, 
				score = newScore,
				runningTotal = rt',
				thirdRoll = thirdRoll}
		in
			-- BUG: technically, if this is the last frame, we have consumed this roll.
			(newFrame, False, rt')

	Complete -> (f, False, case rt of
		Nothing -> Nothing
		Just x -> Just (x + (score f)))

processRoll :: [Frame] -> Int -> [Frame]
processRoll fs roll =
	reverse $ (\(a,b,c) -> a) $ foldl op ([], False, Just 0) fs
	where
		op (fs', bail, rt) f =
			if bail then
				(f : fs', bail, rt)
			else
				(f' : fs', bail', rt')
			where
				(f', bail', rt') = applyRollToFrame f roll rt

processRolls :: [Int] -> [Frame]
processRolls rolls =
	let
		fs = [Frame fn ReadyForFirstRoll 0 Nothing Nothing Nothing Nothing | fn <- [1..maxFrames]]
	in
		foldl (\fs' roll -> processRoll fs' roll) fs rolls
