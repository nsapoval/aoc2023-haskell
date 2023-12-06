module Main where
import Data.List.Split
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

tuplify3 :: [a] -> (a, a, a)
tuplify3 [x, y, z] = (x, y, z)

buildMap :: String -> [(Int, Int, Int)]
buildMap = map (tuplify3 . map read . take 3 . splitOn " ") . tail . splitOn "\n" 

checkRange :: Int -> (Int, Int, Int) -> Maybe Int
checkRange x (v, k, l)
    | k <= x && x < k + l  = Just (v + x - k)
    | otherwise            = Nothing

applyMap :: Int -> [(Int, Int, Int)] -> Int
applyMap x m = if null result then x else head result where 
    result = mapMaybe (checkRange x) m

getSeeds :: String -> [Int]
getSeeds = mapMaybe readMaybe . splitOn " "

getAllMaps :: [String] -> [[(Int, Int, Int)]]
getAllMaps = map buildMap

foldThroughMaps :: Int -> [[(Int, Int, Int)]] -> Int
foldThroughMaps = foldl applyMap

computeFinalValue :: [String] -> Int
computeFinalValue (s:m) = minimum $ map (\x -> foldThroughMaps x (getAllMaps m)) seeds where 
    seeds = getSeeds s 

main :: IO ()
main = do
    contents <- getContents
    let values = splitOn "\n\n" contents
    let result = computeFinalValue values 
    putStr . show $ result