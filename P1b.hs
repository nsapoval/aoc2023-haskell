module Main where
import Data.Char (isDigit, digitToInt)

replaceDigits :: String -> [Int]
replaceDigits [] = []
replaceDigits (c:cs)
    | c == 'o' && take 2 cs == "ne"   = 1 : replaceDigits cs
    | c == 't' && take 2 cs == "wo"   = 2 : replaceDigits cs
    | c == 't' && take 4 cs == "hree" = 3 : replaceDigits cs
    | c == 'f' && take 3 cs == "our"  = 4 : replaceDigits cs
    | c == 'f' && take 3 cs == "ive"  = 5 : replaceDigits cs
    | c == 's' && take 2 cs == "ix"   = 6 : replaceDigits cs
    | c == 's' && take 4 cs == "even" = 7 : replaceDigits cs
    | c == 'e' && take 4 cs == "ight" = 8 : replaceDigits cs
    | c == 'n' && take 3 cs == "ine"  = 9 : replaceDigits cs
    | isDigit c                       = digitToInt c : replaceDigits cs
    | otherwise                       = replaceDigits cs                        

getFirstDigit :: String -> Int
getFirstDigit = head . replaceDigits 

getLastDigit :: String -> Int
getLastDigit = last . replaceDigits 

parseValue :: String -> Int
parseValue s = 10 * getFirstDigit s + getLastDigit s

computeFinalValue :: [String] -> Int
computeFinalValue = sum . map parseValue

main :: IO ()
main = do
    contents <- getContents
    let values = lines contents
    let result = computeFinalValue values 
    putStr . show $ result
