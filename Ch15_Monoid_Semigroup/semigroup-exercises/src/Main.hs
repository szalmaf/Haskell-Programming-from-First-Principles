{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Semigroup
import Control.Monad
import Test.QuickCheck
import GHC.Generics

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

data Three a b c = Three a b c deriving (Eq, Show)
instance Semigroup (Three a b c) where
  x <> y = x
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftM3 Three arbitrary arbitrary arbitrary
type ThreeAssoc a b c = Three a b c -> Three a b c -> Three a b c -> Bool

data Four a b c d = Four a b c d deriving (Eq, Show)
instance Semigroup (Four a b c d) where
  x <> y = x
instance (Arbitrary a, 
          Arbitrary b, 
          Arbitrary c, 
          Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = liftM4 Four arbitrary arbitrary arbitrary arbitrary
type FourAssoc a b c d = 
      Four a b c d
  ->  Four a b c d 
  ->  Four a b c d 
  ->  Bool

newtype BoolConj =
  BoolConj Bool deriving (Eq, Show)
instance Semigroup BoolConj where
  BoolConj True  <> BoolConj True  = BoolConj True
  BoolConj False <> _              = BoolConj False
  _              <> BoolConj False = BoolConj False
instance Arbitrary BoolConj where
  arbitrary = fmap BoolConj arbitrary 
type BoolConjAssoc =
      BoolConj 
  ->  BoolConj
  ->  BoolConj
  ->  Bool

newtype BoolDisj =
  BoolDisj Bool deriving (Eq, Show)
instance Semigroup BoolDisj where
  BoolDisj False <> BoolDisj False = BoolDisj False
  BoolDisj True  <> _              = BoolDisj True
  _              <> BoolDisj True  = BoolDisj True
instance Arbitrary BoolDisj where
  arbitrary = fmap BoolDisj arbitrary
type BoolDisjAssoc = 
      BoolDisj 
  ->  BoolDisj
  ->  BoolDisj
  ->  Bool

data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)
instance Semigroup (Or a b) where
  (Snd x) <> _       = Snd x
  (Fst x) <> (Snd y) = Snd y
  (Fst x) <> (Fst y) = Fst y
instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = frequency [(1, fmap Fst arbitrary), (1, fmap Snd arbitrary)]
type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool


data Bool' =
    True'
  | False'
  deriving (Generic)
instance CoArbitrary Bool'
trueGen :: Gen Int
trueGen = coarbitrary True' arbitrary
falseGen :: Gen Int
falseGen = coarbitrary False' arbitrary

  
newtype Combine a b =
  Combine { unCombine :: (a -> b) }
  -- deriving (Show)
instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine $ (f <> g)
-- instance CoArbitrary (Combine a b) where

newtype Comp a =
  Comp {unComp :: (a -> a)}
instance Semigroup a => Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp $ (f <> g)
-- f = Comp $ \n -> (2*n+1); g = Comp $ \n -> (n-1)
-- unComp (f <> g) $ (Sum 1)
-- Sum {getSum = 3}

data Validation a b =
    Failure' a
  | Success' b
  deriving (Eq, Show)
instance Semigroup a => Semigroup (Validation a b) where
  Success' x <> Success' y = Success' x -- Is it good enough???? b is not Semigroup
  Failure' x <> Success' y = Failure' x
  Success' x <> Failure' y = Failure' y
  Failure' x <> Failure' y = Failure' $ (x <> y)


main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: (IdentityAssoc Int))
  -- verboseCheck (semigroupAssoc :: (TwoAssoc Int String))
  quickCheck (semigroupAssoc :: (TwoAssoc Int String))
  quickCheck (semigroupAssoc :: (ThreeAssoc Int Double String))
  verboseCheck (semigroupAssoc :: (FourAssoc Double Int Int Int))
  verboseCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc:: OrAssoc Int Double)
  -- let f = Combine $ \n -> Sum (n + 1)
  -- let g = Combine $ \n -> Sum (n - 1)
  -- f 5
  -- g 5

