{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Semigroup
import Control.Monad
import Test.QuickCheck
import GHC.Generics



instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = fmap Sum arbitrary

-- Semigroup exercises

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

data Trivial = Trivial deriving (Eq, Show)
instance Semigroup Trivial where
  _ <> _ = Trivial
instance Arbitrary Trivial where
  arbitrary = return Trivial
type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a deriving (Eq, Show)
instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity (x <> y)  
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary
type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

data Two a b = Two a b deriving (Eq, Show)
instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two x1 x2 <> Two y1 y2 = Two (x1 <> y1) (x2 <> y2) 
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
--  (Combine f) <> (Combine g) = Combine $ \x -> (f x <> g x)
instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where -- due to Adam
  arbitrary = fmap Combine arbitrary
type CombineAssoc a b = Combine a b -> Combine a b -> Combine a b -> Bool

newtype Comp a =
  Comp { unComp :: (a -> a) }
instance Semigroup a => Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp $ (f <> g)
-- f = Comp $ \n -> (2*n+1); g = Comp $ \n -> (n-1)
-- unComp (f <> g) $ (Sum 1)
-- Sum {getSum = 3}

data Validation a b = -- Why a and b are diff?? Both could be String
    Failure' a
  | Success' b
  deriving (Eq, Show)
instance Semigroup a => Semigroup (Validation a b) where
  Success' x <> Success' y = Success' x -- Is it good enough???? b is not Semigroup
  Failure' x <> Success' y = Failure' x
  Success' x <> Failure' y = Failure' y
  Failure' x <> Failure' y = Failure' $ (x <> y)

newtype AccumulateRight a b = -- a and b are separately semigroups
  AccumulateRight (Validation a b)
  deriving (Eq, Show)
instance Semigroup b => Semigroup (AccumulateRight a b) where
  AccumulateRight (Success' x) <> AccumulateRight (Success' y) = AccumulateRight (Success' $ (x <> y)) 
  AccumulateRight (Failure' x) <> AccumulateRight (Success' y) = AccumulateRight (Failure' x)
  AccumulateRight (Success' x) <> AccumulateRight (Failure' y) = AccumulateRight (Failure' y)
  AccumulateRight (Failure' x) <> AccumulateRight (Failure' y) = AccumulateRight (Failure' y)

newtype AccumulateBoth a b = -- a and b are separately semigroups
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)
instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  AccumulateBoth (Success' x) <> AccumulateBoth (Success' y) = AccumulateBoth (Success' $ (x <> y)) 
  AccumulateBoth (Failure' x) <> AccumulateBoth (Success' y) = AccumulateBoth (Failure' x)
  AccumulateBoth (Success' x) <> AccumulateBoth (Failure' y) = AccumulateBoth (Failure' y)
  AccumulateBoth (Failure' x) <> AccumulateBoth (Failure' y) = AccumulateBoth (Failure' $ (x <> y))



-- Monoid exercises

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity x = 
  mappend x mempty == x
monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity x =
  mappend mempty x == x

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
  mempty = Combine mempty -- $ \x -> mempty -- fn that maps any x to mempty of b
  mappend = (<>)

instance (Semigroup a, Monoid a) => Monoid (Comp a) where
  mempty = Comp $ \x -> mempty
  mappend = (<>)

-- instance Semigroup a => Semigroup (Mem s a) where
--   f <> g = Mem $ \x ->
--       let s' = snd $ runMem f s
--       in ((fst $ runMem f s) <> (fst $ runMem g s'),
--           snd $ runMem g s')

newtype Mem s a =
  Mem { runMem :: s -> (a,s) } 
instance Semigroup a => Semigroup (Mem s a) where
  Mem f <> Mem g = Mem $ \s -> ((fst . g $ s) <> (fst . f . snd . g $ s)
                              , (snd . f . snd . g $ s))
  -- Mem f <> Mem g = Mem $ \s -> ((fst . f $ s) <> (fst . g . snd . f $ s)
  --                             , (snd . g . snd . f $ s))
  -- Mem f <> Mem g = Mem $ ((fst . f) <> (fst . g . snd . f), (snd . g . snd . f))
  -- Mem f <> Mem g = Mem $ \s -> ((fst $ f s) <> (fst $ g (snd $ f s)), -- Is this part assoc???
  --                   snd $ g (snd $ f s)) -- This is assoc; could be fg instead of gf
instance (Semigroup a, Monoid a) => Monoid (Mem s a) where
  mempty  = Mem $ \s -> (mempty, s) -- a is monoid with mempty elem, and -> is monoid
  mappend = (<>)
f' = Mem $ \s -> ("hi", s + 1)


main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: (IdentityAssoc (Sum Int)))
  -- verboseCheck (semigroupAssoc :: (TwoAssoc Int String))
  quickCheck (semigroupAssoc :: (TwoAssoc (Sum Int) String))
  quickCheck (semigroupAssoc :: (ThreeAssoc Int Double String))
  -- verboseCheck (semigroupAssoc :: (FourAssoc Double Int Int Int))
  -- verboseCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: (FourAssoc Double Int Int Int))
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc Int Double)

  -- quickCheck (semigroupAssoc :: CombineAssoc String Int)

  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (monoidLeftIdentity :: Identity [Int] -> Bool)
  quickCheck (monoidRightIdentity :: Identity [Int] -> Bool)
  quickCheck (monoidLeftIdentity :: Two (Sum Int) [Int] -> Bool)
  quickCheck (monoidRightIdentity :: Two (Sum Int) [Int] -> Bool)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  
  print $ runMem (f' <> mempty) 0
  print $ runMem (mempty <> f') 0
  print $ (runMem mempty 0 :: (String, Int))
  print $ runMem (f' <> mempty) 0 == runMem f' 0
  print $ runMem (mempty <> f') 0 == runMem f' 0



