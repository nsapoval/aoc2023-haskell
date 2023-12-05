module Main where
import Data.List.Split
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe, catMaybes)
import Data.Char (isDigit)
import Data.List

partCriteria :: Char -> Bool
partCriteria x = x /= '.' && not (isDigit x)

gearCriteria :: Char -> Bool
gearCriteria = (==) '*'

getLocalIndices :: (Char -> Bool) -> Int -> String -> [Int]
getLocalIndices criteria offset = map (+ offset) . findIndices criteria 

getFullIndices :: (Char -> Bool) -> Int -> [String] -> [Int]
getFullIndices _ _ [] = []
getFullIndices criteria offset (x:xs) = getLocalIndices criteria offset x ++ getFullIndices criteria (offset + length x) xs

getPartIndices :: [String] -> [Int]
getPartIndices = getFullIndices gearCriteria 0 

getNeighborList :: Int -> Int -> [Int]
getNeighborList base x 
    | x `mod` base == 0        = [x, x + 1, x - base, x - base + 1, x + base, x + base + 1]
    | x `mod` base == base - 1 = [x, x - 1, x - base, x - base - 1, x + base, x + base + 1]
    | otherwise                = [x, x + 1, x - 1, x - base, x - base - 1, x - base + 1, x + base, x + base - 1, x + base + 1]

addNeighborhood :: Int -> Int -> [Int]
addNeighborhood base x = filter (\y ->  y >= 0 && y < base * base) $ getNeighborList base x

getNumberIndices :: [String] -> [Int]
getNumberIndices = getFullIndices isDigit 0

takeRange :: Int -> Int -> [Int] -> ([Int], [Int])
takeRange _ x [] = ([x], [])
takeRange base x (y:ys)
    | y == x + 1 && y `mod` base /= 0  = (x : fst (takeRange base y ys), snd (takeRange base y ys))
    | otherwise                        = ([x], y:ys)

groupNumberIndices :: Int -> [Int] -> [[Int]]
groupNumberIndices _ [] = []
groupNumberIndices base (x:xs) = r : groupNumberIndices base t 
    where (r, t) = takeRange base x xs 

getValues :: String -> [Int]
getValues = mapMaybe readMaybe . splitOneOf ".!@#$%^&*()_+-=/|"

getPartNeighborhoods :: [String] -> [[Int]]
getPartNeighborhoods s = map (addNeighborhood (length s)) . getPartIndices $ s

getGears :: [String] -> [[Int]]
getGears s = filter (\x -> length (filter (any (`elem` x)) ranges) == 2) neighborhoods where
    ranges = groupNumberIndices (length s) (getNumberIndices s)
    neighborhoods = getPartNeighborhoods s

getValueGear :: [Int] -> [[Int]] -> [Int] -> Int
getValueGear values ranges neighborhood =  product . catMaybes $ zipWith (\x y -> if any (`elem` x) neighborhood then Just y else Nothing) ranges values

computeFinalValue :: [String] -> Int
computeFinalValue s = sum (map (getValueGear values ranges) neighborhoods) where 
    values = concatMap getValues s
    ranges = groupNumberIndices (length s) (getNumberIndices s)
    neighborhoods = getGears s

main :: IO ()
main = do
    contents <- getContents
    let values = lines contents
    let result = computeFinalValue values 
    putStr . show $ result