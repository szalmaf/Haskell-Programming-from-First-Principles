-- 7.4 Pattern matching

-- Pattern match on numbers
isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False -- Note the "anything else" _ Pattern

-- Pattern matcing against data constructors
newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser
          | RegisteredUser Username AccountNumber

printUser :: User -> IO()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name)
                          (AccountNumber acctNum))
          = putStrLn $ name ++ " " ++ show acctNum

-- printUser function is called based on pattern matching 
-- on values/constructors of User type. Can it be 
-- dynamic binding?? 
myUser = (Username "Callen")
myAcct =  (AccountNumber 10456)
--printUser $ RegisteredUser myUser myAcct

--Pattern matching on tuples
f :: (a,b) -> (c,d) -> ((b,d), (a,c))
f (a,b) (c,d) = ((b,d), (a,c))

addEmUp2 :: Num a => (a,a) -> a
addEmUp2 (x, y) = x + y

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x


-- 7.5 Case expressions

-- return "yes" of "no"
palin xs =
    case y of
        True  -> "yes"
        False -> "no"
    where y = xs == reverse xs

-- return printed message:
greetIfCool :: String -> IO()
greetIfCool coolness = 
    case cool of
        True  -> putStrLn "eyyyy..."
        False -> putStrLn "pshhhh"
    where cool = coolness == "downright"


-- 7.6 Higher order functions

data Employee = Coder
              | Manager
              | Veep
              | CEO
              deriving (Eq, Ord, Show)
reportBoss :: Employee -> Employee -> IO()
reportBoss e e' =
    putStrLn $ show e ++ " is the boss of " ++ show e'
employeeRank :: Employee -> Employee -> IO()
employeeRank e e' =
    case compare e e' of
        GT -> reportBoss e e'
        EQ -> putStrLn "Neither employee is the boss"
        LT -> (flip reportBoss) e e'
-- extend employeeRank function with a compare fn
-- so that employeeRank becomes a higher order fn
employeeRank' f e e' =
    case f e e' of
        GT -> reportBoss e e'
        EQ -> putStrLn "Neither employee is the boss"
        LT -> (flip reportBoss) e e'
-- define a custom f compare fn to make Coders the boss
codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _     = GT
codersRuleCEOsDrool _ Coder     = LT
--codersRuleCEOsDrool e e'        = compare e e'
-- use cases
--employeeRank' compare Coder CEO
--employeeRank' codersRuleCEOsDrool Coder CEO


-- 7.7 Guards

bloodNa :: Integer -> String
bloodNa x
    | x < 135   = "too low"
    | x > 145   = "too high"
    | otherwise = "just right"

isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c
    | a^2 + b^2 == c^2 = "RIGHT ON"
    | otherwise        = "not right"
    

