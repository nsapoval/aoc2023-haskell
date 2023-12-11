module Main where
import Data.List.Split
import Data.Maybe (mapMaybe, fromMaybe)
import Text.Read (readMaybe)
import qualified Data.Map as Map
import Data.List (sortBy)
import Data.Ord

cardValues = Map.fromList [('2', 1), ('3', 2), ('4', 3), ('5', 4), ('6', 5), ('7', 6), ('8', 7),
    ('9', 8), ('T', 9), ('J', 0), ('Q', 10), ('K', 11), ('A', 12)]

parseHandToValue :: String -> Int
parseHandToValue s = sum $ zipWith (\x y -> x * 13 ^ y) (mapMaybe (`Map.lookup` cardValues) s) (reverse [0..4])

frequenciesWithJ :: [Char] -> (Map.Map Char Int, Int)
frequenciesWithJ s = (Map.update (\x -> Just 0) 'J' fqs, fromMaybe 0 . Map.lookup 'J' $ fqs) where 
    fqs = Map.fromListWith (+) . (`zip` [1,1..]) $ s

sortedFrequencies :: String -> [Int]
sortedFrequencies s = zipWith (+) (jCount : [0, 0..]) (sortBy (comparing Data.Ord.Down) . map snd . Map.assocs $ fqs) where 
   (fqs,  jCount) = frequenciesWithJ s
   
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