type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =  NameEmpty
                    | AgeTooLow
                    | PersonInvalidUnknown String
                    deriving (Eq, Show)

mkPerson :: Name
        ->  Age
        ->  Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == ""            = Left NameEmpty
    | not (age > 0)         = Left AgeTooLow
    | otherwise             = Left $ PersonInvalidUnknown $
                                    "Name was: " ++ show name ++
                                    " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
        putStrLn "Person's name?"
        name <- getLine
        putStrLn "Person's age?"
        ageStr <- getLine
        let age = read ageStr
        let p = mkPerson name age
        case p == (Left _) of
          | False -> putStrLn "Yay! Successfully got a person:" ++ show p
          | True  -> putStrLn " "
        putStrLn $ show age
