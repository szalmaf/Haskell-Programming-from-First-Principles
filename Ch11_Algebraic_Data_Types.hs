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

-- 3.
:k Doggies String
Doggies String :: *

-- 4.
:t Husky 10
Husky 10 :: Num a => Doggies a

-- 5.
Husky (10 :: Integer)
Husky 10

:t Husky (10 :: Integer)
Husky (10 :: Integer) :: Doggies Integer

-- 6.
:t Mastiff "Scooby Doo"
Mastiff "Scooby Doo" :: Doggies [Char]

-- 7.
:t DogueDeBordeaux
DogueDeBordeaux :: doge -> DogueDeBordeaux doge

-- 8.
:t DogueDeBordeaux  "doggie!"
DogueDeBordeaux  "doggie!" :: DogueDeBordeaux [Char]

-- 9.
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
instance TooMany (Int, Int) where
    tooMany (i1, i2) = tooMany (i1 + i2)

3.
-- a has Num and TooMany constrains
-- (a, a) is the input of this TooMany instance
instance TooMany (Num a, TooMany a) => (a, a) where -- a has Num and TooMany constrains
    tooMany(n, x) = tooMany (n + x)

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


-- 11.10 Normal form

data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show
data BookType =   FictionBook Fiction 
                | NonfictionBook Nonfiction 
                deriving Show
type AuthorName = String
data Author  = Author (AuthorName, BookType)
--  Fiction, Nonfiction are different below from above
data Author = 
      Fiction AuthorName
    | Nonfiction AuthorName
    deriving (Eq, Show)

-- Strict interpretation of normal form
type Number = Int
type Add = (Expr Expr)
type Minus = Expr
type Mult = (Expr Expr)
type Divide = (Expr Expr)
type Expr =
    Either Number
      (Either Add
        (Either Minus
          (Either Mult Divide)))

data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show
type Gardener = String
data Garden =
    Garden Gardener FlowerType
    deriving Show
-- the above Garden in normal form:
data Garden = Gardenia Gardener
            | Daisy Gardener
            | Rose Gardener
            | Lilac Gardener
            deriving Show


-- 11.11 Constructing and deconstructing values

data GuessWhat =
    chickenbutt deriving (Eq, Show)

data Id a =
    MkId a deriving (Eq, Show)

data Product a b =
    Product a b deriving (Eq, Show)

data Sum a b =
      First a
    | Second b
    deriving (Eq, Show)

data RecordProduct a b =
    RecordProduct { pfirst :: a 
                  , psecond :: b }
                  deriving (Eq, Show) 

newtype numCow =
    NumCow Int
    deriving (Eq, Show)

newtype NumPig =
    NumPig Int
    deriving (Eq, Show)

data Farmhouse =
    Farmhouse NumCow NumPig
    deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep =
    NumSheep Int
    deriving (Eq, Show)

data BigFarmhouse =
    BigFarmhouse NumCow NumPig NumSheep
    deriving (Eq, Show)

type BigFarmhouse' =
    Product NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo = 
    CowInfo Name Age LovesMud
    deriving (Eq, Show)

data SheepInfo =
    SheepInfo Name Age PoundsOfWool
    deriving (Eq, Show)

data Animal = 
      Cow CowInfo
    | Pig PigInfo
    | Sheep SheepInfo
    deriving (Eq, Show)

type Animal' =
    Sum CowInfo (Sum PigInfo SheepInfo)


-- 11.13 Higher-kinded datatypes

-- a product type which is a product of four types
data Silly a b c d = MkSilly a b c d deriving Show
:kind Silly
Silly :: * -> * -> * -> * -> *
:kind Silly Int
Silly Int :: * -> * -> * -> *
-- or same as Silly expressed as a tuple
:kind (,,,)
(,,,) * -> * -> * -> * -> *
:kind :: (Int, String, Bool, String)
(Int, String, Bool, String) :: *


-- 11.14 Lists are polymorphic


-- 11.15 Binary Tree

-- recursive data structure as Lists
data BinaryTree a =
      Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right) 
    | b == a = Node left a right 
    | b < a  = Node (insert' b left) a right 
    | b > a  = Node left a (insert' b right)
t1 = insert' 1 Leaf


-- 11.16 Chapter exercises

1.
data Weekday =
      Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
-- a) Yes, Weekday is type with 5 data constructors
-- b) It is not really a Tree
-- c) Weekday is not a product type, it is a sum type
-- d) Weekday takes zero arguments, not five

2.
f Friday = "Miller Time"
-- c) f :: Weekday -> [Char] type

3.
-- Types defined with the data keyword
-- a) do not need to have at least one argument, they can have zero
-- b) yes, must begin with a capital letter
-- c) no, they don't need to be polymorphic
-- d) they *can* be imported form modules

4.
