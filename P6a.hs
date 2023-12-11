module Main where
import Data.List.Split
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

getDistance :: Int -> Int -> Int
getDistance holdTime raceTime = holdTime * raceTime

computeWinningRange :: (Int, Int) -> [Int]
computeWinningRange (timeLimit, distanceToBeat) = filter (> distanceToBeat)
    (zipWith getDistance [1..timeLimit-1] (reverse [1..timeLimit-1]))

splitAndTuplify :: [a] -> [(a, a)]
splitAndTuplify l = zip (take (length l `div` 2) l) (reverse $ take (length l `div` 2) (reverse l))

computeFinalValue :: [Int] -> Int
computeFinalValue = product . map (length . computeWinningRange) . splitAndTuplify

main :: IO ()
main = do
    contents <- getContents
    let values = concatMap (mapMaybe readMaybe . splitOneOf ": ") (splitOn "\n" contents) :: [Int]
    let result = computeFinalValue values 
    putStr . show $ result