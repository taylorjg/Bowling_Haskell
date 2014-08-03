import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Bowling
import System.IO
import Text.Printf

formatFrame :: Frame -> String
formatFrame f =
    "{ " ++
    intercalate ", " [
        (show $ frameNumber f),
        (show $ frameState f),
        (show $ runningTotal f),
        (show $ firstRoll f),
        (show $ secondRoll f),
        (show $ thirdRoll f),
        (show $ bonusBalls f)
    ] ++
    " }"

formatFrames :: [Frame] -> String
formatFrames fs = intercalate "\n" $ map formatFrame fs

combineLines :: [String] -> [String] -> [String]
combineLines lines1 lines2 = zipWith (++) lines1 lines2

addFrameSeperator :: [String] -> [String]
addFrameSeperator lines = combineLines lines [
    "+",
    "|",
    "+",
    "|",
    "|",
    "|",
    "|",
    "|",
    "+"]

formatFrameNumber :: Frame -> Int -> String
formatFrameNumber f fw = printf "%-*d" fw fn
    where fn = frameNumber f

isSpareFrame :: Frame -> Bool
isSpareFrame f =
    twoRolls == maxPins
    where
        twoRolls =
            (fromMaybe 11 $ firstRoll f) +
            (fromMaybe 11 $ secondRoll f)

noRollSymbol = " "
gutterSymbol = "-"
spareSymbol = "/"
strikeSymbol = "X"

formatRoll :: Maybe Int -> String
formatRoll Nothing = noRollSymbol
formatRoll (Just x)
    | x == 0 = gutterSymbol
    | x == maxPins = strikeSymbol
    | x > 0 && x < maxPins = show x
    | otherwise = error "invalid roll!"

formatFirstRoll :: Frame -> String
formatFirstRoll = formatRoll . firstRoll

formatSecondRoll :: Frame -> String
formatSecondRoll f =
    if isSpareFrame f then spareSymbol
    else formatRoll $ secondRoll f

formatThirdRoll :: Frame -> String
formatThirdRoll = formatRoll . thirdRoll

formatRunningTotal :: Frame -> Int -> String
formatRunningTotal f fw =
    case rt of
        Nothing -> replicate fw ' '
        Just x -> printf "%-*d" fw x
    where rt = runningTotal f

formatNormalFrame :: [String] -> Frame -> [String]
formatNormalFrame lines f = combineLines lines [
    "-----",
    "  " ++ fn,
    "-----",
    " |" ++ roll1 ++ "|" ++ roll2,
    " +-+-",
    "     ",
    " " ++ rt,
    "     ",
    "-----"]
    where
        fn = formatFrameNumber f 3
        roll1 = formatFirstRoll f
        roll2 = formatSecondRoll f
        rt = formatRunningTotal f 4

formatLastFrame :: [String] -> Frame -> [String]
formatLastFrame lines f = combineLines lines [
    "-------",
    "  " ++ fn,
    "-------",
    " |" ++ roll1 ++ "|" ++ roll2 ++ "|" ++ roll3,
    " +-+-+-",
    "       ",
    " " ++ rt,
    "       ",
    "-------"]
    where
        fn = formatFrameNumber f 5
        roll1 = formatFirstRoll f
        roll2 = formatSecondRoll f
        roll3 = formatThirdRoll f
        rt = formatRunningTotal f 6

formatFrame2 :: [String] -> Frame -> [String]
formatFrame2 lines f = 
    addFrameSeperator $
    if isLastFrame f
        then formatLastFrame lines f 
        else formatNormalFrame lines f

formatFrames2 :: [Frame] -> [String]
formatFrames2 fs =
    foldl formatFrame2 lines fs
    where
        lines = addFrameSeperator $ replicate 9 ""

chooseRolls :: IO [Int]
chooseRolls = do
    let preDefinedRolls = [
            replicate 20 0,
            [2, 7],
            [2, 8, 1, 2],
            [10, 1, 2],
            [10, 10, 1, 2],
            take 20 $ cycle [9, 0],
            replicate 21 5,
            replicate 10 0 ++ [10, 1, 2] ++ replicate 6 0,
            replicate 10 0 ++ [7, 3, 1, 5] ++ replicate 6 0,
            replicate 18 0 ++ [10, 1, 2],
            replicate 18 0 ++ [7, 3, 1],
            replicate 12 10,
            [10, 7, 3, 7, 2, 9, 1, 10, 10, 10, 2, 3, 6, 4, 7, 3, 3]]
    mapM_ putStrLn $ zipWith (\i rs -> show i ++ ") " ++ show rs) [0..] preDefinedRolls
    putStr "Please choose a list of rolls: "
    hFlush stdout
    choice <- getLine
    return $ preDefinedRolls !! (read choice)

main = do
    rolls <- chooseRolls
    let frames = processRolls rolls
    putStrLn $ formatFrames frames
    mapM_ putStrLn $ formatFrames2 frames
