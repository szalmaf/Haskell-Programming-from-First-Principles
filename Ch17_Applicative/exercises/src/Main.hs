module Main where

import Data.List (elemIndex)
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
  (<*>) (Constant f) (Constant x) = Constant x

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
-- instance Applicative List where
--   pure x = Cons x Nil 
--   _ <*> Nil                   = Nil
--   Nil <*> _                   = Nil
-- --  f <*> (Cons x xs)         = Cons (f x) (f <*> xs)  
--   (Cons f fs) <*> (Cons x xs) = Cons (f x) ((Cons f fs) <*> xs)

main :: IO ()
main = do




  return ()
