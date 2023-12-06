module Main where
import Data.List.Split
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

tuplify2 :: [a] -> (a, a)
tuplify2 [x, y] = (x, y)

tuplify3 :: [a] -> (a, a, a)
tuplify3 [x, y, z] = (x, y, z)

buildMap :: String -> [(Int, Int, Int)]
buildMap = map (tuplify3 . map read . take 3 . splitOn " ") . tail . splitOn "\n" 

checkRange :: Int -> (Int, Int, Int) -> Maybe Int
checkRange x (v, k, l)
    | k <= x && x < k + l  = Just (v + x - k)
    | otherwise            = Nothing

invCheckRange :: Int -> (Int, Int, Int) -> Maybe Int
invCheckRange x (v, k, l)
    | v <= x && x < v + l  = Just (k + x - v)
    | otherwise            = Nothing

applyMap :: Int -> [(Int, Int, Int)] -> Int
applyMap x m = if null result then x else head result where 
    result = mapMaybe (checkRange x) m

invApplyMap :: [(Int, Int, Int)] -> Int -> Int
invApplyMap m x = if null result then x else head result where 
    result = mapMaybe (invCheckRange x) m

listToRanges :: [a] -> [(a, a)]
listToRanges l
    | null l = []
    | otherwise = tuplify2 (take 2 l) : listToRanges (drop 2 l)

isSeed :: Int -> (Int, Int) -> Bool
isSeed x (s, l) = s <= x && x < s + l

getSeeds :: String -> [Int]
getSeeds = concatMap (\(x, y) -> take y [x..]) . listToRanges . mapMaybe readMaybe . splitOn " "

getAllMaps :: [String] -> [[(Int, Int, Int)]]
getAllMaps = map buildMap

foldThroughMaps :: Int -> [[(Int, Int, Int)]] -> Int
foldThroughMaps = foldl applyMap

invFoldThroughMaps :: Int -> [[(Int, Int, Int)]] -> Int
invFoldThroughMaps = foldr invApplyMap

invComputeFinalValue :: [String] -> Int
invComputeFinalValue (s:m) = length . takeWhile (\x -> not $ any (isSeed x) seeds) . map (\x -> invFoldThroughMaps x (getAllMaps m)) $ [0..] where 
    seeds = listToRanges . mapMaybe readMaybe . splitOn " " $ s 

computeFinalValue :: [String] -> Int
computeFinalValue (s:m) = minimum $ map (\x -> foldThroughMaps x (getAllMaps m)) seeds where 
    seeds = getSeeds s 

main :: IO ()
main = do
    contents <- getContents
    let values = splitOn "\n\n" contents
    let result = invComputeFinalValue values 
    putStr . show $ result