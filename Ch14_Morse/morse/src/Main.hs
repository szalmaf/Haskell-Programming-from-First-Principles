module Main where

import Control.Monad (forever, when) 
import Data.List (intercalate)
import Data.Traversable (traverse)
import Morse (stringToMorse, morseToChar) 
import System.Environment (getArgs) 
import System.Exit (exitFailure, exitSuccess)
import System.IO (hGetLine, hIsEOF, stdin)


convertToMorse :: IO () 
convertToMorse = forever $ do
    weAreDone <- hIsEOF stdin 
    when weAreDone exitSuccess
    
    -- otherwise, proceed.
    line <- hGetLine stdin 
    convertLine line

    where
        convertLine line = do
            let morse = stringToMorse line 
            case morse of
                (Just str) -> putStrLn (intercalate " " str) 
                Nothing -> do
                    putStrLn $ "ERROR: " ++ line 
                    exitFailure


convertFromMorse :: IO () 
convertFromMorse = forever $ do
    weAreDone <- hIsEOF stdin 
    when weAreDone exitSuccess
    
    -- otherwise, proceed.
    line <- hGetLine stdin 
    convertLine line
    
    where
        convertLine line = do
            let decoded :: Maybe String 
                decoded =
                        traverse morseToChar
                                 (words line)
            case decoded of
                (Just s) -> putStrLn s 
                Nothing -> do
                    putStrLn $ "ERROR: " ++ line 
                    exitFailure


main :: IO () 
main = do
    mode <- getArgs 
    case mode of
        [arg] ->
                 case arg of
                    "from" -> convertFromMorse 
                    "to"   -> convertToMorse
                    _      -> do -- argError
                            putStrLn "Error!"
                            exitFailure
        _     -> do -- argError
                putStrLn "Error!"
                exitFailure
    -- where argError = do   -- This argError won't work for some reason!!!1
    --     putStrLn "Error!"
    --     exitFailure


