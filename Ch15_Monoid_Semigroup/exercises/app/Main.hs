module Main where

-- import Lib

import Data.Monoid
import Test.QuickCheck

data Optional a =
      Nada
    | Only a
    deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where 
    mempty  = Nada
    mappend Nada (Only x)     = Only x
    mappend (Only x) Nada     = Only x
    mappend (Only x) (Only y) = Only (x <> y)

newtype First' a =  
    First' { getFirst' :: Optional a }
    deriving (Eq, Show)

instance Monoid a => Monoid (First' a) where
    mempty      = First' {getFirst' = Nada}
    mappend x y = First' {getFirst' = (getFirst' x) <> (getFirst' y)} 

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = frequency [(1,  return First' {getFirst' = Nada}), -- Can mempty be used as below??
                           (10, fmap (\x -> First' {getFirst' = Only x}) arbitrary)]
-- instance Arbitrary a => Arbitrary (First' a) where
--     arbitrary = frequency [(1,  return mempty :: (First' a)), 
--                            (10, fmap (\x -> First' {getFirst' = Only x}) arbitrary)]

firstMappend :: Monoid a => First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = 
       First' String 
    -> First' String 
    -> First' String 
    -> Bool

type FstId =
    First' String -> Bool



monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc x y z = x <> (y <> z) == (x <> y) <> z

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity x = (x <> mempty) == x

monoidRightIdentity :: (Eq a, Monoid a) => a -> Bool
monoidRightIdentity x = (mempty <> x) == x 





main :: IO ()
main = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)
