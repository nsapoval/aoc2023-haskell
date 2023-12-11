module Main where
import Data.List.Split
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import qualified Data.Map as Map
import Data.List (sortBy)
import Data.Ord

cardValues = Map.fromList [('2', 0), ('3', 1), ('4', 2), ('5', 3), ('6', 4), ('7', 5), ('8', 6),
    ('9', 7), ('T', 8), ('J', 9), ('Q', 10), ('K', 11), ('A', 12)]

parseHandToValue :: String -> Int
parseHandToValue s = sum $ zipWith (\x y -> x * 13 ^ y) (mapMaybe (`Map.lookup` cardValues) s) (reverse [0..4])

frequencies :: Ord k => [k] -> Map.Map k Int
frequencies = Map.fromListWith (+) . (`zip` [1,1..])

sortedFrequencies :: String -> [Int]
sortedFrequencies = sortBy (comparing Data.Ord.Down) . map snd . Map.assocs . frequencies

parseHandToType :: [Int] -> Int
parseHandToType (x:xs)
    | x == 5                  = 7000000
    | x == 4                  = 6000000
    | x == 3 && head xs == 2  = 5000000
    | x == 3                  = 4000000
    | x == 2 && head xs == 2  = 3000000
    | x == 2                  = 2000000
    | otherwise               = 1000000 

getHandStrength :: [String] -> [Int]
getHandStrength = map (\x -> parseHandToValue x + (parseHandToType . sortedFrequencies $ x)) 

sortHandsAndBids :: [String] ->  [(Int, Int)]
sortHandsAndBids inputs = sortBy (\(x,_) (y,_) -> compare x y) (zip (getHandStrength hands) bids) where
    hands = map (head . splitOn " ") inputs
    bids  = mapMaybe (readMaybe . last . splitOn " ") inputs :: [Int]

computeFinalValue :: [String] -> Int
computeFinalValue inp = sum (zipWith (\x (_, bid) -> x * bid) [1..] (sortHandsAndBids inp))

main :: IO ()
main = do
    contents <- getContents
    let values = splitOn "\n" contents
    let result = computeFinalValue values 
    putStr . show $ result