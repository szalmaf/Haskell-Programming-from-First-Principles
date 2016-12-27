module Main where

import Test.QuickCheck
import Data.List (sort)


---------------

half x = x / 2

halfIdentity = (*2) . half

prop_halfIdentity :: Double -> Bool
prop_halfIdentity x = halfIdentity x == x

---------------

-- data SortedList a =
--   SortedList a
  -- deriving (Ord, Show)

sortedListGen :: (Arbitrary a, Ord a) => Gen [a]
sortedListGen = do
  x <- arbitrary
  return (sort x)

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t)  = (Just y, x >= y)

---------------


main :: IO ()
main = do
  
  quickCheck prop_halfIdentity
  sample (sortedListGen :: Gen [Int])
  quickCheck (listOrdered :: ([Int] -> Bool))