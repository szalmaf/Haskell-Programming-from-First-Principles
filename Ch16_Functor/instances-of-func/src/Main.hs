{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Rank2Types #-} -- for Nat
{-# LANGUAGE FlexibleInstances #-} -- for Flip

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

data Three a b c = Three a b c deriving (Eq, Show)
instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftM3 Three arbitrary arbitrary arbitrary
type ThreeFI = Three [Int] Double Int -> Bool
type ThreeFC = Three [Int] Double Int -> IntToInt -> IntToInt -> Bool

data Three' a b = Three' a b b deriving (Eq, Show)
instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = liftM3 Three' arbitrary arbitrary arbitrary
type Three'FI = Three' [Int] Int -> Bool
type Three'FC = Three' [Int] Int -> IntToInt -> IntToInt -> Bool
 
data Four a b c d = Four a b c d deriving (Eq, Show)
instance Functor (Four a b c) where
  fmap f (Four x y z zz) = Four x y z (f zz)
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where
  arbitrary = liftM4 Four arbitrary arbitrary arbitrary arbitrary
type FourFI = Four Double Integer [Int] Int -> Bool
type FourFC = Four Double Integer [Int] Int -> IntToInt -> IntToInt-> Bool

data Four' a b = Four' a a a b deriving (Eq, Show)
instance Functor (Four' a) where
  fmap f (Four' x y z zz) = Four' x y z (f zz)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = liftM4 Four' arbitrary arbitrary arbitrary arbitrary
type Four'FI = Four' Double Int -> Bool
type Four'FC = Four' Double Int -> IntToInt -> IntToInt-> Bool

-- data Trivial = Trivial's type constructor is not of one parameter one

data Possibly a =
    LolNope
  | Yeppers a
  deriving (Eq, Show)
instance Functor Possibly where
  fmap _ LolNope     = LolNope
  fmap f (Yeppers x) = Yeppers (f x)

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)
instance Functor (Sum a) where
  fmap _ (First x) = First x
  fmap f (Second y) = Second (f y)

-- Functor instance to First or Left is impossible bc 
-- the type arguments have order a b, and we cannot single our a
-- or anything before the last


-- Changing structure and keeping date, instead of 
-- keeping stucture and changing data
type Nat f g = forall a . f a -> g a
maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just x) = [x]

data Quant a b =
    Finance
  | Desk a
  | Bloor b
instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk x)  = Desk x
  fmap f (Bloor y) = Bloor (f y)

data K a b =
  K a
instance Functor (K a) where
  fmap _ (K x) = K x

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)
instance Functor (Flip K a) where
  fmap f (Flip (K x)) = Flip (K (f x))

data EvilGoateeConst a b = -- What is/should be const here??
  GoatyConst b
instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst y) = GoatyConst (f y) 

data LiftItOut f a =
  LiftItOut (f a)
instance Functor f => Functor (LiftItOut f) where
  fmap fn (LiftItOut xs) = LiftItOut (fmap fn xs) -- Note the fmap on th right!!!

data Parappa f g a =
  DaWrappa (f a) (g a)
instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap fn (DaWrappa xs ys) = DaWrappa (fmap fn xs) (fmap fn ys)

data Notorious g o a t =
  Notorious (g o) (g a) (g t)
instance Functor g => Functor (Notorious g o a) where
  fmap fn (Notorious xs ys zs) = Notorious xs ys (fmap fn zs)

data List a =
    Nil
  | Cons  a (List a)
instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

data GoatLord a =
    NoGoat
  | OneGoat a 
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor GoatLord where
  fmap _ NoGoat               = NoGoat
  fmap f (OneGoat x)          = OneGoat (f x)
  fmap f (MoreGoats xs ys zs) = MoreGoats (fmap f xs) (fmap f ys) (fmap f zs)

data TalkToMe a =
    Halt
  | Print String a 
  | Read (String -> a)
instance Functor TalkToMe where
  fmap _ Halt         = Halt
  fmap f (Print s x)  = Print s (f x)
  fmap f (Read fn)    = Read (fmap f fn)


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

  quickCheck (functorIdentity :: ThreeFI)
  quickCheck (functorCompose' :: ThreeFC)

  quickCheck (functorIdentity :: Three'FI)
  quickCheck (functorCompose' :: Three'FC)

  quickCheck (functorIdentity :: FourFI)
  quickCheck (functorCompose' :: FourFC)

  quickCheck (functorIdentity :: Four'FI)
  quickCheck (functorCompose' :: Four'FC)
