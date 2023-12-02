module Main where
import Data.List.Split

data GameConfig = Config Int Int Int 

addConfig :: GameConfig -> GameConfig -> GameConfig
addConfig (Config x y z) (Config x' y' z') = Config (x + x') (y + y') (z + z') 

parseValue :: String -> GameConfig
parseValue s = case last . splitOn " " $ s of
    "red" -> Config (read . last. take 2 . splitOn " " $ s) 0 0
    "green" -> Config 0 (read . last. take 2 . splitOn " " $ s) 0
    "blue" -> Config 0 0 (read . last. take 2 . splitOn " " $ s)

parseCounts :: String -> GameConfig
parseCounts = foldr (addConfig . parseValue) (Config 0 0 0) . splitOn ","

isPossibleConfig :: Int -> Int -> Int -> GameConfig -> Bool
isPossibleConfig red green blue (Config x y z) = (x <= red) && (y <= green) && (z <= blue)

isPossibleGame :: Int -> Int -> Int -> String -> Bool
isPossibleGame red green blue = all (isPossibleConfig red green blue . parseCounts) . splitOn ";" . last . splitOn ":"

getId :: String -> Int
getId = read . last . splitOn " " . head . splitOn ":"

computeFinalValue :: [String] -> Int
computeFinalValue = sum . map getId . filter (isPossibleGame 12 13 14)

main :: IO ()
main = do
    contents <- getContents
    let values = lines contents
    let result = computeFinalValue values 
    putStr . show $ result