module Main where

import Data.Semigroup
import Control.Monad
import Test.QuickCheck

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

data Trivial = Trivial deriving (Eq, Show)
instance Semigroup Trivial where
  _ <> _ = Trivial
instance Arbitrary Trivial where
  arbitrary = return Trivial
type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a deriving (Eq, Show)
instance Semigroup (Identity a) where
  x <> y = x
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary
type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

data Two a b = Two a b deriving (Eq, Show)
instance Semigroup (Two a b) where
  x <> y = x
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = liftM2 Two arbitrary arbitrary
type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool


main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: (IdentityAssoc Int))
  -- verboseCheck (semigroupAssoc :: (TwoAssoc Int String))
  quickCheck (semigroupAssoc :: (TwoAssoc Int String))