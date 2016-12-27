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
  return (sort x) -- generates sorted list
-- sample (sortedListGen :: Gen [Int])

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) (sort xs)
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t)  = (Just y, x >= y)

---------------

plusAssociative x y z =
  x + (y + z) == (x + y) + z
plusCommutative x y =
  x + y == y + x

---------------

powerAssociative x y z =
  x ^ (y ^ z) == (x ^ y) ^ z
powerCommutative x y =
  x ^ y == y ^ x

---------------

doubleReverse x =
  (reverse . reverse $ x) == id x

---------------

propDollar x =
  (id $ x) == x 

---------------

propFoldr1 x y =
  foldr (:) x y == (++) x y
propFoldr2 x = 
  foldr (++) [] x == concat x

---------------

main :: IO ()
main = do
  
  quickCheck prop_halfIdentity

  sample (sortedListGen :: Gen [Int])
  quickCheck (listOrdered :: ([Int] -> Bool))

  quickCheck (plusAssociative :: Int -> Int -> Int -> Bool)
  quickCheck (plusCommutative :: Int -> Int -> Bool)

  quickCheck (powerAssociative :: Int -> Int -> Int -> Bool)
  quickCheck (powerCommutative :: Int -> Int -> Bool)

  quickCheck (doubleReverse :: [String] -> Bool)

  quickCheck (propDollar :: Int -> Bool)

  quickCheck (propFoldr1 :: [String] -> [String] -> Bool)
  quickCheck (propFoldr2 :: [String] -> Bool)