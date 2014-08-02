import Data.List (intercalate)
import Bowling

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

main = do
    let frames = processRolls $ replicate 12 10
    putStrLn $ formatFrames frames
