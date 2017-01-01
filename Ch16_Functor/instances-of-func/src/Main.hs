{-# LANGUAGE ViewPatterns #-}

module Main where

import Test.QuickCheck
import Test.QuickCheck.Function
import Control.Monad


functorIdentity :: (Functor f, Eq (f a)) => 
  f a -> Bool
functorIdentity f = 
  fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
  (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = 
  (fmap g (fmap f x)) == (fmap (g . f) x)
li x = functorCompose (+1) (*2) (x :: [Int]) -- operates on concrete fns
functorCompose' :: (Eq (f c), Functor f) =>
       f a
    -> Fun a b      -- arbitrarily generates f and g fns
    -> Fun b c
    -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)
type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool


newtype Identity a = Identity a deriving (Eq, Show)
instance Functor Identity where
  fmap f (Identity x) = Identity (f x)
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary
type IdentityFI = Identity Int -> Bool
type IdentityFC = Identity Int -> IntToInt -> IntToInt -> Bool

data Pair a = Pair a a deriving (Eq, Show)
instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)
instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = liftM2 Pair arbitrary arbitrary
type PairFI = Pair Int -> Bool
type PairFC = Pair Int -> IntToInt -> IntToInt -> Bool 

data Two a b = Two a b deriving (Eq, Show)
instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = liftM2 Two arbitrary arbitrary
type TwoFI = Two Double Int -> Bool
type TwoFC = Two Double Int -> IntToInt -> IntToInt -> Bool 


main :: IO ()
main = do

  quickCheck $ \x -> functorIdentity (x :: [Int])
  quickCheck li
  quickCheck (functorCompose' :: IntFC)

  quickCheck (functorIdentity :: IdentityFI)
  quickCheck (functorCompose' :: IdentityFC)

  quickCheck (functorIdentity :: PairFI)
  quickCheck (functorCompose' :: PairFC)

  quickCheck (functorIdentity :: TwoFI)
  quickCheck (functorCompose'  :: TwoFC)



