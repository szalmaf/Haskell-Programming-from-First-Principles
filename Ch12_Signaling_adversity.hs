
-- 12.2 Smart constructors for datatypes
import Data.List


-- Simple product type

-- type Name = String
-- type Age = Integer
-- data Person = Person Name Age deriving Show


-- Smart conctructors - only constructs type if they meet criteria

-- type Name = String
-- type Age = Integer
-- data Person = Person Name Age deriving Show
-- mkPerson :: Name -> Age -> Maybe Person
-- mkPerson name age
--     | name /= "" && age >= 0  = Just $ Person name age
--     | otherwise               = Nothing

-- 12.3 Either
-- Either, Left, Right -- create either PersonInvalid or Person
-- Left is the error constructor -- functor won't map over it
-- Right is the valid constructor

-- type Name = String
-- type Age = Integer
-- data Person = Person Name Age deriving Show
-- data PersonInvalid =  NameEmpty
--                     | AgeTooLow
--                     deriving (Eq, Show)
-- mkPerson :: Name -> Age -> Either PersonInvalid Person
-- mkPerson name age
--     | name /= "" && age >= 0  = Right $ Person name age
--     | name == ""              = Left NameEmpty
--     | otherwise               = Left AgeTooLow


-- Separate checking for various error constructors

type Name = String
type Age = Integer
type ValidatePerson a = Either [PersonInvalid] a -- type alias use with 3 different a's later
data Person = Person Name Age deriving Show
data PersonInvalid =  NameEmpty
                    | AgeTooLow
                    deriving (Eq, Show)
ageOkay :: Age -> ValidatePerson Age
ageOkay age = case age >= 0 of
    True  -> Right age
    False -> Left [AgeTooLow]
nameOkay :: Name -> ValidatePerson Name
nameOkay name = case name /= "" of
    True  -> Right name
    False -> Left [NameEmpty]
mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)
mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPerson' (Left badName) _             = Left badName
mkPerson' _              (Left badAge) = Left badAge
--mkPerson "" (-10)
-- The following mkPerson will do the above later
---- mkPerson :: Name -> Age -> Validation [PersonInvalid] Person
---- mkPerson name age = liftA2 Person (nameOkay name) (ageOkay age) -- note the liftA2


-- 12.4 Kinds, higher order types

-- fmap Just [1,2,3]


-- Chapter Exercises


-- Determine the kinds

-- 1. 
-- id :: a -> a
-- 'a' is of kind *.

-- 2.
-- r :: a -> f a
-- 'a' is kind *, while f is kind *, too (??).


-- String processing

-- 1. 
notThe :: String -> Maybe String
notThe x 
    | x == "the" = Nothing
    | x /= "the" = Just x  

replaceThe :: String -> String
replaceThe str
    | length str >= 3  = replaceThe' str 0
    | length str <  3  = str
    where 
        replaceThe' x n -- Utility fn actually doing the job
            | n > length x - 3 = x  -- end of string
            | m == "the"       = replaceThe' (l ++ "a" ++ r) (n + 1)  -- replace "the"
            | otherwise        = replaceThe' x (n + 1)  -- step forw
            where
                l = take n x
                m = take 3 $ drop n x -- take 3 chars at pos n
                r = drop (n+3) x
-- replaceThe "hello the cow loves us the"
-- replaceThe' str = unwords $ map f $ words
-- replaceThe unwords . map ((maybe "a" id) . notThe) . words
-- replaceThe''' = unwords . map f . mapp notThe . words
--     where
--         f (Just s) = s
--         f Nothing  = "a"

-- 2.

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str =
    countTheBeforeVowel' strL cnt
    where
        strL = words str
        cnt = 0
        isVowel c = 
            foldr (||) False tf
            where
                tf = map (c ==) "aeiou"
        countTheBeforeVowel' (w:ws) cnt
            | w == "the" && 
              ws /= [] && 
              isVowel (head $ head ws)  = countTheBeforeVowel' ws cnt+1
            | ws /= []                  = countTheBeforeVowel' ws cnt
            | otherwise                 = cnt

-- 3. 

isVowel :: Char -> Bool
isVowel c = foldr (||) False tf
    where tf = map (c ==) "aeiou"
countVowels :: String -> Integer
countVowels word
    | word == []   = 0  -- guard for empty string 
    | otherwise   = countVowels' word 0
    where
        countVowels' (w:ws) cnt
            | ws == [] && (isVowel w) = cnt + 1
            | ws == []                = cnt
            | ws /= [] && (isVowel w) = countVowels' ws cnt + 1
            | otherwise               = countVowels' ws cnt


-- Validate a word

newtype Word' = Word' String
    deriving (Eq, Show)
vowels = "aeiou"
mkWord :: String -> Maybe Word'
mkWord s 
    | nVowels > nConsonants  = Nothing
    | otherwise              = Just $ Word'     s
    where
        len = toInteger $ length s
        nVowels = countVowels s
        nConsonants = len - nVowels


-- It's only Natural

data Nat =
      Zero
    | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + (natToInteger x)

integerToNat :: Integer -> Maybe Nat
integerToNat n
    | n < 0     = Nothing
    | n == 0    = Just Zero
    | otherwise = Just (integerToNat' n)
    where
        integerToNat' 0 = Zero
        integerToNat' n = Succ (integerToNat' (n-1))

-- Small library for Maybe

-- 1.
isJust :: Maybe a -> Bool
isJust (Just _)  = True
isJust _  = False

isNothing :: Maybe a -> Bool
isNothing (Just _) = False
isNothing Nothing = True

-- 2.
-- Needs (Num a, Num b) typeclass restriction!!!!
mayybee :: (Num a, Num b) => b -> (a -> b) -> Maybe a -> b
mayybee x f Nothing = x + f 0 
mayybee x f (Just y) = x + f y

-- 3.
-- Needs Num a typeclass restriction!!!!
fromMaybe :: Num a => a -> Maybe a -> a
fromMaybe x Nothing = mayybee x (+0) Nothing
fromMaybe x (Just y) = mayybee x (+0) (Just y)

-- 4.
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- 5.
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Just x) : xs) = x : (catMaybes xs)
catMaybes (Nothing : xs)  = catMaybes xs
--catMaybes ls = [x | Just x <- ls]

-- 6.
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Nothing
flipMaybe (Nothing:xs) = Nothing
flipMaybe xs = Just $ flipMaybe' xs
    where flipMaybe' ((Just x):xs) = x:(flipMaybe' xs)
--flipMaybe xs = if any isNothing xs then Nothing else Just (catMaybes xs)
-- flipMaybe xs = case any isNothing xs of
--     True  -> Nothing
--     False -> Just $ catMaybes xs

-- Small library for Either

-- 1.
lefts' :: [Either a b] -> [a]
lefts' xs = foldr f [] xs
    where f (Left x) ys  = x : ys
          f (Right x) ys = ys

-- 2.
rights' :: [Either a b] -> [b]
rights' xs = foldr f [] xs
    where f (Left x) ys  = ys
          f (Right x) ys = x : ys

-- 3.
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = foldr f ([],[]) xs
    where f (Left x) ys  = (x : fst ys, snd ys)
          f (Right x) ys = (fst ys, x : snd ys)

-- 4.
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left _) = Nothing
eitherMaybe' f (Right x) = Just $ f x 

-- 5.
-- Wont work??????
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x)  = f x
either' _ g (Right y) = g y

-- 6.
-- based on 5.???????
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f x = either' (\y -> Nothing) (Just . f) x

-- *unfold* and *iterate*
-- iterate :: (a -> a) -> a -> [a]
-- infoldr :: (b -> Maybe (a, b)) -> b -> [a]

mehSum :: Num a => [a] -> a
mehSum xs = go 0 xs
    where go :: Num a => a -> [a] -> a
          go n [] = n
          go n (x:xs) = go (n+x) xs
niceSum :: Num a => [a] -> a
niceSum = foldl' (+) 0

mehProduct :: Num a => [a] -> a
mehProduct xs = go 1 xs
    where go :: Num a => a -> [a] -> a
          go n [] = n
          go n (x:xs) = go (n*x) xs
niceProduct :: Num a => [a] -> a
niceProduct = foldl' (*) 1

mehConcat :: [[a]] -> [a]
mehConcat xs = go [] xs
    where go :: [a] -> [[a]] -> [a]
          go xs' [] = xs'
          go xs' (x:xs) = go (xs' ++ x) xs
niceConcat :: [[a]] -> [a]
niceConcat = foldr (++) []

-- 1.
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : (myIterate f $ f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
    Nothing     -> []
    Just (y,z)  -> y : (myUnfoldr f z) 

myUnfoldr' f x = go $ f x
    where go Nothing = []
          go $ Just (a,b) = a : go $ f b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr g x
    where g y = Just (y, f y)









