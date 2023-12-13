import System.IO
import Data.Char (isDigit)

data Card = Card {
    cardId :: Int,
    winningNumbers :: [Int],
    guessedNumbers :: [Int]
}

instance Show Card where
    show c = "Card { id = " ++ show (cardId c) ++ ", winningNumbers = " ++ show (winningNumbers c) ++ ", guessedNumbers = " ++ show (guessedNumbers c) ++ " }"

increaseCountBy :: Int -> Int -> [Int] -> [Int]
increaseCountBy _ 0 xs = xs
increaseCountBy _ _ [] = []
increaseCountBy byHowMuch howMany (x:xs) = (x + byHowMuch) : increaseCountBy byHowMuch (howMany - 1) xs

getId :: String -> Int
getId = read . takeWhile isDigit . dropWhile (not . isDigit)

parseCard :: String -> Card
parseCard cardStr = Card (getId cardPart) (getWinningNumbers numberPart) (getGuessedNumbers numberPart)
    where (cardPart, numberPartWithColon) = break (==':') cardStr
          numberPart = drop 1 numberPartWithColon

getWinningNumbers :: String -> [Int]
getWinningNumbers card = map read . words $ winningSide
    where (winningSide, _) = break (=='|') card

getGuessedNumbers :: String -> [Int]
getGuessedNumbers card = map read . words $ drop 1 guessedSide
    where (_, guessedSide) = break (=='|') card

overlap :: [Int] -> [Int] -> [Int]
overlap [] _ = []
overlap _ [] = []
overlap (x:xs) ys
    | x `elem` ys = x : overlap xs ys
    | otherwise = overlap xs ys

score :: [Int] -> Int
score [] = 0
score xs = 2^exponent
    where exponent = length xs - 1

cardScore :: Card -> Int
cardScore c = score $ overlap (winningNumbers c) (guessedNumbers c)

updateCardCounts :: [Card] -> [Int] -> [Int]
updateCardCounts [] _ = []
updateCardCounts _ [] = []
updateCardCounts (c:cs) (x:xs) = x : updateCardCounts cs newCounts
    where newCounts = increaseCountBy x (length $ overlap (winningNumbers c) (guessedNumbers c)) xs


main :: IO ()
main = do
    fileHandle <- openFile "data/04.in" ReadMode
    contents <- hGetContents fileHandle
    -- uncomment for part 1
    -- print $ sum $ map cardScore . map parseCard . lines $ contents
    -- uncomment for part 2
    let cards = map parseCard $ lines contents
    print $ sum $ updateCardCounts cards (take (length cards) $ repeat 1)
    hClose fileHandle

