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
    show (Puzzle _ discovered guessed) =
        (intersperse ' ' $ fmap renderPuzzleChar discovered)
        ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle wrd = Puzzle wrd mapwrd lst
    where mapwrd = 
          lst    = []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle wrd _ _) c =
    elem c wrd

alreadyGuessed (Puzzle _ _ guessed) c =
    elem c guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar mc = case mc of
    Nothing  -> '_'
    Just c   -> c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
    Puzzle word newFilledInSoFar (c : s)
    where zipper guessed wordChar guessChar =
            if wordChar == guessed
            then Just wordChar
            else guessChar
          newFilledInSoFar = 
              zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess
        , alreadyGuessed puzzle guess) of
     (_, True) -> do
         putStrLn "You already guessed that character, pick somethign else!"
         return puzzle
     (True, _) -> do
         putStrLn "This character was in the word, filling in the word accordingly"
         return (fillInCharacter puzzle guess)
     (False, _) -> do
         putStrLn "This character wasn't in the word, try again."
         return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
    if (length guessed) > 7 then
        do putStrLn "You lose!"
           putStrLn $ "The word was: " ++ wordToGuess
           exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
    if all isJust filledInSoFar then
        do putStrLn "You win!"
            exitSuccess
        else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStrLn "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _   -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle

