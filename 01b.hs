import System.IO


extractDigitsFromString :: String -> String
extractDigitsFromString [] = []
extractDigitsFromString ('o':'n':'e':xs) = '1' : extractDigitsFromString ('e':xs)
extractDigitsFromString ('t':'w':'o':xs) = '2' : extractDigitsFromString ('o':xs)
extractDigitsFromString ('t':'h':'r':'e':'e':xs) = '3' : extractDigitsFromString ('e':xs)
extractDigitsFromString ('f':'o':'u':'r':xs) = '4' : extractDigitsFromString ('r':xs)
extractDigitsFromString ('f':'i':'v':'e':xs) = '5' : extractDigitsFromString ('e':xs)
extractDigitsFromString ('s':'i':'x':xs) = '6' : extractDigitsFromString ('x':xs)
extractDigitsFromString ('s':'e':'v':'e':'n':xs) = '7' : extractDigitsFromString ('n':xs)
extractDigitsFromString ('e':'i':'g':'h':'t':xs) = '8' : extractDigitsFromString ('t':xs)
extractDigitsFromString ('n':'i':'n':'e':xs) = '9' : extractDigitsFromString ('e':xs)
extractDigitsFromString (x:xs)
    | x `elem` ['0'..'9'] = x : extractDigitsFromString xs
    | otherwise = extractDigitsFromString xs

getFirstAndLast :: String -> String
getFirstAndLast [] = []
getFirstAndLast [x] = [x, x]
getFirstAndLast (x:xs) = [x, last xs]

getNumberFromLine :: String -> Int
getNumberFromLine = read . getFirstAndLast . extractDigitsFromString

main :: IO ()
main = do
    fileHandle <- openFile "data/01_01_input.txt" ReadMode
    contents <- hGetContents fileHandle
    print $ sum $ map getNumberFromLine $ lines contents
    hClose fileHandle
    
