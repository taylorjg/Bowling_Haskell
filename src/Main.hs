import Data.List (intercalate)
import Bowling
import System.IO
import Text.Printf

type Line = String
type Lines = [Line]

formatFrame1 :: Frame -> Line
formatFrame1 f =
    "{ " ++
    intercalate ", " [
        (show $ frameNumber f),
        (show $ runningTotal f),
        (show $ firstRoll f),
        (show $ secondRoll f),
        (show $ thirdRoll f)
    ] ++
    " }"

formatFrames1 :: Frames -> Lines
formatFrames1 = map formatFrame1

formatFrameNumber :: Frame -> Int -> String
formatFrameNumber f fw = printf "%-*d" fw fn
    where fn = frameNumber f

noRollSymbol = " "
gutterSymbol = "-"
spareSymbol = "/"
strikeSymbol = "X"

formatRoll :: Maybe Roll -> String
formatRoll Nothing = noRollSymbol
formatRoll (Just x)
    | x == 0 = gutterSymbol
    | x == maxPins = strikeSymbol
    | x > 0 && x < maxPins = show x
    | otherwise = error $ "formatRoll: invalid roll (" ++ show x ++ ")"

formatFirstRoll :: Frame -> String
formatFirstRoll = formatRoll . firstRoll

formatSecondRoll :: Frame -> String
formatSecondRoll f = if isSpareFrame f then spareSymbol else formatRoll $ secondRoll f

formatThirdRoll :: Frame -> String
formatThirdRoll = formatRoll . thirdRoll

formatRunningTotal :: Frame -> Int -> String
formatRunningTotal f fw =
    case runningTotal f of
        Nothing -> replicate fw ' '
        Just x -> printf "%-*d" fw x

combineLines :: Lines -> Lines -> Lines
combineLines lines1 lines2 = zipWith (++) lines1 lines2

addFrameSeperator :: Lines -> Lines
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

formatNormalFrame :: Frame -> Lines -> Lines
formatNormalFrame f lines = combineLines lines [
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

formatLastFrame :: Frame -> Lines -> Lines
formatLastFrame f lines = combineLines lines [
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

formatFrame2 :: Lines -> Frame -> Lines
formatFrame2 lines f =
    addFrameSeperator $
    if isLastFrame f
        then formatLastFrame f lines
        else formatNormalFrame f lines

formatFrames2 :: Frames -> Lines
formatFrames2 fs =
    foldl formatFrame2 lines fs
    where
        lines = addFrameSeperator emptyLines
        emptyLines = replicate 9 ""

chooseRolls :: IO Rolls
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
            [10, 7, 3, 7, 2, 9, 1, 10, 10, 10, 2, 3, 6, 4, 7, 3, 3],
            [8, 5],
            [8, 1, 7, 6],
            [8, 1, 13, 4],
            replicate 13 10]
    mapM_ putStrLn $ zipWith (\i rs -> show i ++ ") " ++ show rs) [0..] preDefinedRolls
    putStr "Please choose a list of rolls: "
    hFlush stdout
    choice <- getLine
    return $ preDefinedRolls !! (read choice)

main = do
    rolls <- chooseRolls
    case processRolls rolls of
        Left be -> putStrLn be
        Right frames -> do
            mapM_ putStrLn $ formatFrames1 frames
            mapM_ putStrLn $ formatFrames2 frames
