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



