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

-- 10.6 Data constructor arities

-- nullary
data Example0 =
    Example0 deriving (Eq, Show)

-- unary
data Example1 =
    Example1 = Int deriving (Eq, Show)

-- product of Int String
data Example2 =
    Example2 Int String deriving (Eq, Show)

Example1 10 == Example1 11 -- False
Example2 10 "FlappityBat" == Example2 1 "NC" -- False



