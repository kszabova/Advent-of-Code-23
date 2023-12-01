import System.IO

isDigit :: Char -> Bool
isDigit = (`elem` ['0'..'9'])

getDigits :: String -> [Char]
getDigits = filter isDigit

concatFirstAndLast :: [Char] -> [Char]
concatFirstAndLast [] = []
concatFirstAndLast [x] = [x, x]
concatFirstAndLast (x:xs) = [x, last xs]

getNumberFromLine :: String -> Int
getNumberFromLine = read . concatFirstAndLast . getDigits

main :: IO ()
main = do
    fileHandle <- openFile "data/01_01_input.txt" ReadMode
    contents <- hGetContents fileHandle
    print $ sum $ map getNumberFromLine $ lines contents
    hClose fileHandle
