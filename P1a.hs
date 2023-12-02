module Main where
import Data.Char (isDigit, digitToInt)

getFirstDigit :: String -> Int
getFirstDigit = digitToInt . head . filter isDigit 

getLastDigit :: String -> Int
getLastDigit = digitToInt . last . filter isDigit

parseValue :: String -> Int
parseValue s = 10 * getFirstDigit s + getLastDigit s

computeFinalValue :: [String] -> Int
computeFinalValue = sum. map parseValue

main :: IO ()
main = do
    contents <- getContents
    let values = lines contents
    let result = computeFinalValue values 
    putStr . show $ result
