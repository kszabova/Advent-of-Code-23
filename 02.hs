import System.IO
import Data.Char (isSpace, isDigit)

data GameSet = GameSet {
    red :: Int,
    blue :: Int,
    green :: Int
}

instance Show GameSet where
    show gs = "GameSet { red = " ++ show (red gs) ++ ", blue = " ++ show (blue gs) ++ ", green = " ++ show (green gs) ++ " }"

data Game = Game {
    gameNumber :: Int,
    gameSets :: [GameSet]
}

instance Show Game where
    show g = "Game { gameNumber = " ++ show (gameNumber g) ++ ", gameSets = " ++ show (gameSets g) ++ " }"

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

getGame :: String -> Game
getGame s = Game (getGameNumber gameNameStr) (getGameSets gameSetsStr)
    where (gameNameStr:gameSetsStr:[]) = wordsWhen (== ':') s

getGameSetsStrs :: String -> [String]
getGameSetsStrs = map trim . wordsWhen (== ';')

getBallCounts :: String -> [String]
getBallCounts = map trim . wordsWhen (== ',')

getGameSets :: String -> [GameSet]
getGameSets = map createGameSet . getGameSetsStrs

updateGameSet :: GameSet -> String -> GameSet
updateGameSet gs [] = gs
updateGameSet gs stringSet = case color of
    "red" -> gs { red = red gs + (read count :: Int) }
    "blue" -> gs { blue = blue gs + (read count :: Int) }
    "green" -> gs { green = green gs + (read count :: Int) }
    _ -> gs
    where (count:color:[]) = words stringSet

createGameSet :: String -> GameSet
createGameSet = foldl updateGameSet (GameSet 0 0 0) . getBallCounts

getGameNumber :: String -> Int
getGameNumber = read . dropWhile (not . isDigit)

isValidGameSet :: GameSet -> Bool
isValidGameSet gs = (red gs <= 12) && (green gs <= 13) && (blue gs <= 14)

isValidGame :: Game -> Bool
isValidGame = all isValidGameSet . gameSets

minViableGameSet :: [GameSet] -> GameSet
minViableGameSet gss = GameSet (maximum $ map red gss) (maximum $ map blue gss) (maximum $ map green gss)

gameSetPower :: GameSet -> Int
gameSetPower gs = (red gs) * (blue gs) * (green gs)

main :: IO ()
main = do
    fileHandle <- openFile "data/02.in" ReadMode
    contents <- hGetContents fileHandle
    -- uncomment for part 1
    -- print $ sum $ map gameNumber $ filter isValidGame $ map getGame $ lines contents
    -- uncomment for part 2
    print $ sum $ map (gameSetPower . minViableGameSet . gameSets . getGame) $ lines contents
    hClose fileHandle

