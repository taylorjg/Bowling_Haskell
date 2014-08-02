import Data.List (intercalate)
import Bowling
import System.IO

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

chooseRolls :: IO [Int]
chooseRolls = do
    let preDefinedRolls = [
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
    choice <- getLine
    return $ preDefinedRolls !! (read choice)

main = do
    rolls <- chooseRolls
    let frames = processRolls rolls
    putStrLn $ formatFrames frames
