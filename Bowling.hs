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
	nextRolls :: [Int],
	runningTotal :: Maybe Int,
	firstRoll :: Maybe Int,
	secondRoll :: Maybe Int,
	thirdRoll :: Maybe Int}
	deriving Show

isStrike :: Int -> Bool
isStrike roll = roll == maxPins

isLastFrame :: Frame -> Bool
isLastFrame f = frameNumber f == maxFrames

frameScore :: Frame -> Int
frameScore f =
	(fromMaybe 0 $ firstRoll f) +
	(fromMaybe 0 $ secondRoll f) +
	(fromMaybe 0 $ thirdRoll f) +
	(sum $ nextRolls f)

formatFrame :: Frame -> String
formatFrame f =
	"{ " ++
	intercalate ", " [
		(show $ frameNumber f),
		(show $ frameState f),
		(show $ nextRolls f),
		(show $ runningTotal f),
		(show $ firstRoll f),
		(show $ secondRoll f),
		(show $ thirdRoll f)
	] ++
	" }"

formatFrames :: [Frame] -> String
formatFrames fs = intercalate "\n" $ map formatFrame fs

applyRollToFrame :: Frame -> Int -> Maybe Int -> (Frame, Bool, Maybe Int)
applyRollToFrame f roll rt = case frameState f of

	ReadyForFirstRoll ->
		let
			newFrameState = if isStrike roll then StrikeNeedTwoMore else ReadyForSecondRoll
			newFrame = f { 
				frameState = newFrameState, 
				firstRoll = Just roll}
		in
			(newFrame, True, Nothing)

	ReadyForSecondRoll ->
		let
			newScore = (fromJust $ firstRoll f) + roll
			isSpare = newScore == maxPins
			newFrameState = if isSpare then SpareNeedOneMore else Complete
			rt' = rt >>= (\x -> if isSpare then Nothing else Just (x + newScore))
			newFrame = f { 
				frameState = newFrameState, 
				runningTotal = rt',
				secondRoll = Just roll}
		in
			(newFrame, True, rt')

	SpareNeedOneMore ->
		let
			consumedBall = isLastFrame f
			thirdRoll = if consumedBall then Just roll else Nothing
			newNextRolls = [roll]
			newScore = maxPins + sum newNextRolls
			newFrameState = Complete
			rt' = rt >>= (\x -> Just (x + newScore))
			newFrame = f { 
				frameState = newFrameState, 
				nextRolls = newNextRolls,
				runningTotal = rt',
				thirdRoll = thirdRoll}
		in
			(newFrame, consumedBall, rt')

	StrikeNeedTwoMore ->
		let
			consumedBall = isLastFrame f
			secondRoll = if consumedBall then Just roll else Nothing
			newNextRolls = [roll]
			newScore = maxPins + sum newNextRolls
			newFrameState = StrikeNeedOneMore
			newFrame = f { 
				frameState = newFrameState, 
				nextRolls = newNextRolls,
				secondRoll = secondRoll}
		in
			(newFrame, consumedBall, Nothing)

	StrikeNeedOneMore ->
		let
			consumedBall = isLastFrame f
			thirdRoll = if consumedBall then Just roll else Nothing
			newNextRolls = (nextRolls f) ++ [roll]
			newScore = maxPins + sum newNextRolls
			newFrameState = Complete
			rt' = rt >>= (\x -> Just (x + newScore))
			newFrame = f { 
				frameState = newFrameState, 
				nextRolls = newNextRolls,
				runningTotal = rt',
				thirdRoll = thirdRoll}
		in
			(newFrame, consumedBall, rt')

	Complete -> (f, False, rt >>= (\x -> Just (x + frameScore f)))

processRoll :: [Frame] -> Int -> [Frame]
processRoll fs roll =
	reverse $ (\(a, _, _) -> a) $ foldl op ([], False, Just 0) fs
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
		fs = [Frame fn ReadyForFirstRoll [] Nothing Nothing Nothing Nothing | fn <- [1..maxFrames]]
	in
		foldl (\fs' roll -> processRoll fs' roll) fs rolls

main = do
	putStrLn $ formatFrames $ processRolls $ replicate 12 10
