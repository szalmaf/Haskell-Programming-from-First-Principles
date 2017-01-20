{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE ApplicativeDo #-}

module Main where

import Data.List (elemIndex)
import Control.Applicative (liftA3)
import BadMonoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- List applicative and lookup in map structures
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])

y0 :: Maybe Integer
y0 = lookup 4 $ zip [1,2,3] [4,5,6]
z0 :: Maybe Integer
z0 = lookup 2 $ zip [1,2,3] [4,5,6]
tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y0 <*> z0

x1 :: Maybe Int
x1 = elemIndex 4 [1,2,3,4,5]
y1 :: Maybe Int
y1 = elemIndex 4 [1,2,3,4,5]
max' :: Int -> Int -> Int
max' = max
maxed :: Maybe Int
maxed = max' <$> x1 <*> y1

xs2 = [1,2,3]
ys2 = [4,5,6]
x2 :: Maybe Integer
x2 = lookup 3 $ zip xs2 ys2
y2 :: Maybe Integer
y2 = lookup 2 $ zip xs2 ys2
summed :: Maybe Integer
summed = sum <$> ((,) <$> x2 <*> y2) -- probably something else is expected

-- Identity applicative
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)
instance Functor Identity where 
  fmap f (Identity x) = Identity (f x)
instance Applicative Identity where
  pure x = Identity x
  (<*>) (Identity f) (Identity x) = Identity (f x)

-- Constant applicative
newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)
instance Functor (Constant a) where
  fmap f (Constant x) = Constant x
instance Monoid a => Applicative (Constant a) where
  pure x = Constant mempty -- This is a tricky one!!!
  (<*>) (Constant x) (Constant y) = Constant (x <> y)

-- Maybe applicative
a = const <$> Just "Hello" <*> (pure "World" :: Maybe String)
b = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> (pure [1, 2, 3] :: Maybe [Int])

-- List Applicative Exercise
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs) 
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys -- recursive step
fold :: (a -> b -> b) -> b -> List a -> b
fold _ x Nil = x -- nothing to fold but the init value
fold f x (Cons y ys) = f y (fold f x ys)
concat' :: List (List a) -> List a
concat' = fold append Nil
flatMap :: (a -> List b) -> List a -> List b
flatMap f xs = concat' $ fmap f xs
instance Applicative List where
  pure x = Cons x Nil 
  fs <*> xs = flatMap fn fs where -- This is an awesome compact
                fn f = fmap f xs  -- solution

-- ZipList Applicative Exercise
take' :: Int -> List a -> List a
take' _ Nil         = Nil
take' 0 _           = Nil
take' x (Cons y ys) = Cons y (take' (x-1) ys)

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)
instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs -- pattern match to get 
                in take' 3000 l       -- l as List
          ys' = let (ZipList' l) = ys 
                in take' 3000 l
instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs
instance Applicative ZipList' where
  pure x                     = ZipList' (pure x)
  _                    <*> ZipList' Nil         = ZipList' Nil
  ZipList' Nil         <*> _                    = ZipList' Nil
  ZipList' (Cons f fs) <*> ZipList' (Cons x xs) =
        ZipList' (Cons (f x) (fs'xs)) where
          ZipList' fs'xs = ZipList' fs <*> ZipList' xs
-- toMyList = foldr Cons Nil
-- z = ZipList' $ toMyList  [(+9), (*2), (+8)]
-- z' = ZipList' $ toMyList  [1..3]
-- z <*> z'

-- Variation on Either exercise: Validation
data Validation e a =
    Failure' e
  | Success' a
  deriving (Eq, Show)
instance Functor (Validation e) where
  fmap f (Success' x)  = Success' (f x) 
  fmap _ (Failure' x)  = Failure' x -- Failure' x' case
instance Monoid e => Applicative (Validation e) where
  pure x = Success' x -- ??????
  (Success' f) <*> (Success' x) = Success' (f x) -- functorial?!
  (Success' x) <*> (Failure' y) = Failure' y
  (Failure' x) <*> (Success' y) = Failure' x
  (Failure' x) <*> (Failure' y) = Failure' (mappend x y) -- monoidal!!!
instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = frequency [(1, fmap Success' arbitrary), (1, fmap Failure' arbitrary)]
instance (Eq e, Eq a) => EqProp (Validation e a) where 
  (=-=) = eq 
-- quickBatch  $ applicative (undefined :: Validation  String (Int, Double, Char))
-- See also: http://stackoverflow.com/questions/36009335/how-do-i-test-this-applicative-instance-with-checkers-no-instance-for-coarbitr

-- Chapter exercises

-- Specialize Applicative functions pure and (<*>)
-- pure :: Applicative f => a -> f a
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- pure :: a -> [] a
-- (<*>) :: [] (a -> b) -> [] a -> [] b

-- pure :: a -> IO a
-- (<*>) :: IO (a -> b) -> IO a -> IO b

-- Need the monoid restriction on the first variable 'a'
-- pure :: Monoid a => a -> (,) a a
-- (<*>) :: Monoid a => (,) a (a -> b) -> (,) a a -> (,) a b

-- No need for monoid restriction; but use 'e' as first type variable
-- pure :: a -> (->) e a
-- (<*>) :: (->) a (a -> b) -> (->) a a -> (->) a b

-- Write applicative instances for the following datatypes:

data Pair a = Pair a a deriving (Eq, Show)
instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)
instance Applicative Pair where
  pure x = Pair x x -- :: Monoid a =>  a -> Pair a
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y) 
instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary
instance Eq a => EqProp (Pair a) where
  (=-=) = eq

data Two a b = Two a b deriving (Eq, Show)
instance Functor (Two a) where
  fmap f (Two x y ) = Two x (f y)
instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  (Two x g) <*> (Two y z) = Two (mappend x y) (g z)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary
instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

data Three a b c = Three a b c deriving (Eq, Show)
instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)
instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  (Three x y f) <*> (Three z1 z2 z3) = 
          Three (mappend x z1) (mappend y z2) (f z3)
instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
      Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary
instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

data Three' a b = Three' a b b deriving (Eq, Show)
instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)
instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x -- Is this x x correct???
  (Three' x f g) <*> (Three' z1 z2 z3) = Three' (mappend x z1) (f z2) (g z3)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary
instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

data Four a b c d = Four a b c d deriving (Eq, Show)
instance Functor (Four a b c) where
  fmap f (Four x1 x2 x3 x) = Four x1 x2 x3 (f x) 
instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  (Four x1 x2 x3 f) <*> (Four y1 y2 y3 y) = 
        Four (mappend x1 y1) (mappend x2 y2) (mappend x3 y3) (f y)
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

data Four' a b = Four' a a a b deriving (Eq, Show)
instance Functor (Four' a) where
  fmap f (Four' x1 x2 x3 x) = Four' x1 x2 x3 (f x)
instance Monoid a => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  (Four' x1 x2 x3 f) <*> (Four' y1 y2 y3 y) = 
        Four' (mappend x1 y1) (mappend x2 y2) (mappend x3 y3) (f y)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq


-- Combinations
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a,b,c)]
combos xs ys zs = (,,) <$> xs <*> ys <*> zs



main :: IO ()
main = do

  quickBatch $ applicative (undefined :: Validation  String (Int, Double, Char))
  quickBatch $ applicative (undefined :: Pair (Int, Double, String))
  quickBatch $ applicative (undefined :: Two String (Int, Double, String))
  quickBatch $ applicative (undefined :: Three String String (Int, Double, String))
  quickBatch $ applicative (undefined :: Three' String (Int, Double, String))
  quickBatch $ applicative (undefined :: Four [Int] String [Int] (Int, Double, String))
  quickBatch $ applicative (undefined :: Four' [Int] (Int, Double, String))



  return ()
