module Main where
import Data.List.Split

data GameConfig = Config Int Int Int 

addConfig :: GameConfig -> GameConfig -> GameConfig
addConfig (Config x y z) (Config x' y' z') = Config (x + x') (y + y') (z + z') 

maxConfig :: GameConfig -> GameConfig -> GameConfig
maxConfig (Config x y z) (Config x' y' z') = Config (max x x') (max y y') (max z z') 

parseValue :: String -> GameConfig
parseValue s = case last . splitOn " " $ s of
    "red" -> Config (read . last. take 2 . splitOn " " $ s) 0 0
    "green" -> Config 0 (read . last. take 2 . splitOn " " $ s) 0
    "blue" -> Config 0 0 (read . last. take 2 . splitOn " " $ s)

parseCounts :: String -> GameConfig
parseCounts = foldr (addConfig . parseValue) (Config 0 0 0) . splitOn ","

minCostGame :: String -> GameConfig
minCostGame = foldr (maxConfig . parseCounts) (Config 0 0 0) . splitOn ";" . last . splitOn ":"

getGameValue :: GameConfig -> Int
getGameValue (Config x y z) = x * y * z

computeFinalValue :: [String] -> Int
computeFinalValue = sum . map (getGameValue . minCostGame)

main :: IO ()
main = do
    contents <- getContents
    let values = lines contents
    let result = computeFinalValue values 
    putStr . show $ result