
-- Smart constructors for datatypes


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
replaceThe x
    | length x >= 3  = replaceThe' x 0
    | length x <  3  = x
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

-- 2.