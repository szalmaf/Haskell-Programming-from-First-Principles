module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

main :: IO ()
main = putStrLn "Hello, Haskell!"


type WordList = [String]

allWords :: IO WordList
allWords = do
    dict <- readFile "/usr/share/dict/words"
    return (lines dict)

minWordLength :: Int
minWordLength = 5
maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
    aw <- allWords
    return $ filter gameLength aw

randomWord wl = do 
    randomIndex <- randomRIO (0, length wl - 1)
    return $ wl !! randomIndex

randomWord' = gameWords >>= randomWord

-- Product of to-be-guessed word, discovered chars, 
-- other guessed chars
data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
    Show = (Puzzle _ discovered guessed) =
        (intersperse ' ' $ fmap renderPuzzleChar discovered)
        ++ " Guessed so far: " ++ guessed



