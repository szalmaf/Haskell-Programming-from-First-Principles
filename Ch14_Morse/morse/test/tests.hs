module Main where 

import qualified Data.Map as M
import Morse
import Test.QuickCheck

allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
    forAll charGen
    (\c -> ((charToMorse c)
        >>= morseToChar) == Just c)

----------------

data Trivial =
    Trivial
    deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen =
    return Trivial

instance Arbitrary Trivial where
    arbitrary = trivialGen

----------------

data Identity a =
    Identity a
    deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
    a <- arbitrary
    return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = identityGen

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

----------------

data Pair a b = 
    Pair a b
    deriving (Eq, Show)

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
    a <- arbitrary
    b <- arbitrary
    return (Pair a b)

instance ( Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = pairGen

pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen

main :: IO ()
main = do
    quickCheck prop_thereAndBackAgain
    sample trivialGen
    sample identityGenInt
    sample pairGenIntString


