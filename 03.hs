import System.IO
import Data.Char (isDigit)

data Number = Number {
    number :: Int,
    indices :: [Int]
}

instance Show Number where
    show n = "Number { number = " ++ show (number n) ++ ", indices = " ++ show (indices n) ++ " }"

data Symbol = Symbol {
    symbol :: Char,
    index :: Int
}

instance Show Symbol where
    show s = "Symbol { symbol = " ++ show (symbol s) ++ ", index = " ++ show (index s) ++ " }"

data Line = Line {
    numbers :: [Number],
    symbols :: [Symbol]
}

processLine :: String -> Line
processLine s = Line (extractNumbersFromLine s 0) (extractSymbolsFromLine s 0)

extractSymbolsFromLine :: String -> Int -> [Symbol]
extractSymbolsFromLine "" _ = []
extractSymbolsFromLine (x:xs) index
    | (x == '.') || (isDigit x) = extractSymbolsFromLine xs (index + 1)
    | otherwise = Symbol x index : extractSymbolsFromLine xs (index + 1)

extractNumbersFromLine :: String -> Int -> [Number]
extractNumbersFromLine "" _ = []
extractNumbersFromLine (x:xs) index
    | isDigit x = Number (read number :: Int) [index..end] : extractNumbersFromLine rest (end + 1)
    | otherwise = extractNumbersFromLine xs (index + 1)
    where (number, rest) = span isDigit (x:xs)
          end = index + (length number) - 1

isPartNumber :: [Symbol] -> Number -> Bool
isPartNumber [] n = False
isPartNumber (x:xs) n
    | (index x) `elem` [start..end] = True
    | otherwise = isPartNumber xs n
    where start = (head $ indices n) - 1
          end = (last $ indices n) + 1

getPartNumbers :: [Symbol] -> [Number] -> [Number]
getPartNumbers symbols = filter (isPartNumber symbols)

getPartNumbersFromLines :: Line -> [Line] -> [Number]
getPartNumbersFromLines prev [] = []
getPartNumbersFromLines prev (cur:[]) = getPartNumbers (symbols prev ++ symbols cur) (numbers cur)
getPartNumbersFromLines prev (cur:next:xs) = getPartNumbers (symbols prev ++ symbols cur ++ symbols next) (numbers cur) ++ getPartNumbersFromLines cur (next:xs)

getGearCandidateAdjacents :: [Number] -> Symbol -> [Number]
getGearCandidateAdjacents ns s = filter (\n -> (index s) `elem` [(head $ indices n) - 1..(last $ indices n) + 1]) ns

getGearCandidateAdjacentsFromLine :: Line -> [Number] -> [[Number]]
getGearCandidateAdjacentsFromLine l ns = map (getGearCandidateAdjacents ns) (filter (\s -> (symbol s) == '*') (symbols l))

getGearAdjacentsFromLines :: Line -> [Line] -> [[Number]]
getGearAdjacentsFromLines prev [] = []
getGearAdjacentsFromLines prev (cur:[]) = getGearCandidateAdjacentsFromLine cur (numbers cur ++ numbers prev)
getGearAdjacentsFromLines prev (cur:next:xs) = getGearCandidateAdjacentsFromLine cur (numbers cur ++ numbers prev ++ numbers next) ++ getGearAdjacentsFromLines cur (next:xs)

getGearAdjacents :: [[Number]] -> [[Number]]
getGearAdjacents = filter (\n -> length n == 2)

getProd :: [Number] -> Int
getProd = product . map number

main :: IO ()
main = do
    fileHandle <- openFile "data/03.in" ReadMode
    contents <- hGetContents fileHandle
    -- uncomment for part 1
    -- print $ sum $ map number $ getPartNumbersFromLines (Line [] []) (map processLine $ lines contents)
    -- uncomment for part 2
    print $ sum $ map getProd $ getGearAdjacents $ getGearAdjacentsFromLines (Line [] []) (map processLine $ lines contents)
    hClose fileHandle
