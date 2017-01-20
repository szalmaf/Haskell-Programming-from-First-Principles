instance Functor Chan where 
    fmap f (Chan xs) = Chan (map f xs)

