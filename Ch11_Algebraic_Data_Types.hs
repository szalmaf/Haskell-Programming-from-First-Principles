-- Haskell Data types:
--    1. sum types
--    2. product types
--    3. product types with record syntax
--    4. type aliases
--    5. "newtype"  


-- 11.2 Data declarations

-- Sum types, with more than one constructor
data Bool = False | True -- Bool type
data [] a = [] | a : [a] -- List type


-- 11.3 Data and type constructor

-- Similarly to how to check the *types* of functions w :t
-- f function takes no argument
let f = not True
:t f
f :: Bool

-- f function w one argument
let f x = x > 3
:t f
f :: (Ord a, Num a) => a -> Bool

-- One can check *kinds* of types w :kinds
:k Bool
Bool :: *

-- or
:k [Int]
[Int] :: *


--  11.4 Data constructors and values

data PugType = PugData -- const type & const value
myPug = PugData :: PugType

data HuskyType a = HuskyData -- a is *phantom* argument 
myHusky :: HuskyType a
myHusky = HuskyData

data DogueDeBordeaux doge = DogueDeBordeaux doge
myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

data Doggies a =
      Husky a 
    | Mastiff a
    deriving (Eq, Show)
:k Doggies -- kind of the type
Doggies :: * -> * -- type needs to be applied to become concrete
:t Husky -- type of Husky
Husky :: a -> Doggies a

-- Intermission exercises
:k Doggies String
Doggies String :: *

:t Husky 10
Husky 10 :: Num a => Doggies a

Husky (10 :: Integer)
Husky 10

:t Husky (10 :: Integer)
Husky (10 :: Integer) :: Doggies Integer

:t Mastiff "Scooby Doo"
Mastiff "Scooby Doo" :: Doggies [Char]

:t DogueDeBordeaux
DogueDeBordeaux :: doge -> DogueDeBordeaux doge

:t DogueDeBordeaux  "doggie!"
DogueDeBordeaux  "doggie!" :: DogueDeBordeaux [Char]

:k DogueDeBordeaux
DogueDeBordeaux :: * -> *


-- 11.5 What's a type and what's data?

data Price =
    Price Integer deriving (Eq, Show)

-- type w 3 possible values
data Manufacturer =
      Mini
    | Mazda
    | Tata
      deriving (Eq, Show)

-- type w 3 possible values
data Airline =
      PapuAir
    | CatapultsR'Us
    | TakeYourChancesUnited
      deriving (Eq, Show)

-- type w 2 possible values
-- and dataconstructors taking two and one arguments 
data Vehicle = Car Manufacturer Price
             | Plane Airline
             deriving (Eq, Show)

-- Intermission exercises

myCar :: Vehicle
myCar = Car Mini (Price 14000)

-- Pattern matching to check type
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) -> True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars [(Car _ _)] = ???

getManu :: Vehicle -> Manufacturer
getManu (Car x) = x
getManu _ = ???

data Vehicle = Car Manufacturer Price
             | Plane Airline (Size :: Integer)
             deriving (Eq, Show)

-- 11.6 Data constructor arities

-- nullary
data Example0 =
    Example0 deriving (Eq, Show)

-- unary
data Example1 =
    Example1 = Int deriving (Eq, Show)

-- product of Int and String
-- tuple or anonymous product
data Example2 =
    Example2 Int String deriving (Eq, Show)

Example1 10 == Example1 11 -- False
Example2 10 "FlappityBat" == Example2 1 "NC" -- False


-- 11.7 What makes these datatypes algebraic

-- cardinality of datatypes

-- Simple datatypes with nullary constructor
data Example = MakeExample deriving Show
-- cardinality 1

-- Unary constructors
data Goats = Goats Int deriving (Eq, Show)

-- newtype
-- define a typeclass and its instance for Int
class TooMany a where 
    tooMany :: a -> Bool
instance TooMany Int where 
    tooMany n = n > 42
tooMany  (42 :: Int)

newtype Goats = Goats Int deriving Show
instance TooMany Goats where
    tooMany (Goats n) = n > 43

---------- Use language *pragma* ----
-- no need to define instance of TooMany for Goats due to pragma
{-# LANGUAGE  GeneralizeNewtypeDeriving #-} -- at top of file
class TooMany a where 
    tooMany :: a -> Bool
instance TooMany Int where 
    tooMany n = n > 42
newtype Goats =
    Goats Int deriving (Eq, Show, TooMany)  

-- Intermission exercises
1 . 
{-# LANGUAGE  FlexibleInstances #-} -- at top of file
instance TooMany (Int, String) where
    tooMany (i, s) = i > 42

2.
instance tooMany (Int, Int) where
    tooMany (i1, i2) = tooMany (i1 + i2)

3.
instance tooMany (Num a, TooMany a)
    => (a, a) ?????

-- 11.8 Sum types

data BigSmall =
      Big Bool
    | Small Bool
    deriving (Eq, Show)
-- cardiality is 2 + 2 = 4

import Data.Int
data NumberOrBool =
      Numba Int8
    | BoolyBool Bool
    deriving (Eq, Show)
let myNumba = Numba (-128)
-- cardinality is 256 + 2 = 258

-- 11.9 Product types

-- tuples are anonymous product types
-- Records
data Person = 
    Person { name :: String
           , age :: Int } 
           deriving (Eq, Show)
let papu = Person "Papu" 5
age papu
name papu

--Intermission exercises

-- 2. 4.
data Fruit =
      Peach
    | Plum
    | Apple
    | Blackberry
    deriving (Ord, Eq, Show)
data JamJars =
    Jam { fruit :: Fruit
        , count :: Int } 
        deriving (Ord, Eq, Show)

-- 5.
row1 = JamJars Peach 4
row2 = JamJars Apple 15
row3 = JamJars Plum 6
alljam = [row1, row2, row3]

-- 6.
totalJam x = sum (map count x) -- count total number of jars

-- 7.
maxCnt x y = if count x > count y then x else y
pickMax (x:xs) = foldr maxCnt x xs

-- 8.
:i sortBy
sortBy :: (a -> a -> Ordering) -> [a] -> [a]

:i groupBy
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]

-- 9.
compareKind (JamJars k _) (JamJars k' _) = compare k k'
sortBy compareKind  alljam

-- 10.
groupBy (\x y -> (fruit x) == (fruit y)) alljam