module Main where
import Data.List.Split
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

getNumList :: String -> [Int]
getNumList = mapMaybe readMaybe . splitOn " " 

getCardValue :: String -> Int
getCardValue s = sum . map (const 1) . filter (`elem` targetList) $ givenList 
    where [targetList, givenList] = map getNumList . take 2 . splitOn "|" $ s

computeAllCardValues :: [String] -> [Int]
computeAllCardValues = map (getCardValue . last . splitOn ":") 

addToFirst :: Int -> Int -> [Int] -> [Int]
addToFirst v n l = map (+v) (take n l) ++ drop n l

foldWithTake :: [Int] -> [Int] -> Int
foldWithTake [] _ = 0
foldWithTake (x:xs) (y:ys) = y + foldWithTake xs (addToFirst y x ys)

computeFinalValue :: [String] -> Int
computeFinalValue s = foldWithTake (computeAllCardValues s) (repeat 1)

main :: IO ()
main = do
    contents <- getContents
    let values = lines contents
    let result = computeFinalValue values 
    putStr . show $ result