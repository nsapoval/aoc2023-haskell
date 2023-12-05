module Main where
import Data.List.Split
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

getNumList :: String -> [Int]
getNumList = mapMaybe readMaybe . splitOn " " 

getCardValue :: String -> Int
getCardValue s = sum . map (const 1) . filter (`elem` targetList) $ givenList 
    where [targetList, givenList] = map getNumList . take 2 . splitOn "|" $ s

computeFinalValue :: [String] -> Int
computeFinalValue = sum . map (2^) . filter (>=0) . map ((+ (-1)) . getCardValue . last . splitOn ":") 

main :: IO ()
main = do
    contents <- getContents
    let values = lines contents
    let result = computeFinalValue values 
    putStr . show $ result