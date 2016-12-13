module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

-- main :: IO ()
-- main = putStrLn "Hello, Haskell!"


newtype WordList = 
    WordList [String]
    deriving (Show, Eq)

allWords :: IO WordList
allWords = do
    dict <- readFile "/usr/share/dict/words"
    return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5
maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
    (WordList aw) <- allWords
    return $ WordList (filter gameLength aw)
    where gameLength w =
            let l = length (w :: String)
            in l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do 
    randomIndex <- randomRIO (0, (length wl) - 1)
    return $ wl !! randomIndex

randomWord' = gameWords >>= randomWord

-- Product of to-be-guessed word, discovered chars, 
-- other guessed chars
data Puzzle = Puzzle String [Maybe Char] [Char] Integer

instance Show Puzzle where
    show (Puzzle _ discovered guessed nWrong) =
        (intersperse ' ' $ fmap renderPuzzleChar discovered)
        ++ " Guessed so far: " ++ guessed
        ++ " Wrong guesses so far: " ++ show nWrong

freshPuzzle :: String -> Puzzle
freshPuzzle wrd = Puzzle wrd mapwrd lst nWrong
    where mapwrd = take (length wrd) (repeat Nothing)
          lst    = []
          nWrong = 0

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle wrd _ _ _) c =
    elem c wrd

alreadyGuessed (Puzzle _ _ guessed _) c =
    elem c guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar mc = case mc of
    Nothing  -> '_'
    Just c   -> c

fillInCharacter :: Puzzle -> Char -> Integer -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s nW) c nWrong =
    Puzzle word newFilledInSoFar (c : s) newnW
    where zipper guessed wordChar guessChar =
            if wordChar == guessed
            then Just wordChar
            else guessChar
          newFilledInSoFar = 
              zipWith (zipper c) word filledInSoFar
          newnW = nW + nWrong

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess
        , alreadyGuessed puzzle guess) of
     (_, True) -> do
         putStrLn "You already guessed that character, pick somethign else!"
         return puzzle
     (True, _) -> do
         putStrLn "This character was in the word, filling in the word accordingly"
         return (fillInCharacter puzzle guess 0)
     (False, _) -> do
         putStrLn "This character wasn't in the word, try again."
         return (fillInCharacter puzzle guess 1)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ _ nWrong) =
    if nWrong > 7 then
        do putStrLn "You lose!"
           putStrLn $ "The word was: " ++ wordToGuess
           exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) =
    if all isJust filledInSoFar then
        do putStrLn "You win!"
           exitSuccess
        else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    hSetBuffering stdout NoBuffering
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _   -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle

