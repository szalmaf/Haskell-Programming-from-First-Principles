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