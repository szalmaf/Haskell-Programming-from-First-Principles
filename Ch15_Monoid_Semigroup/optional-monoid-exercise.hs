import Data.Monoid

data Optional a =
      Nada
    | Only a
    deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where 
    mempty  = Nada
    mappend Nada (Only x)     = Only x
    mappend (Only x) Nada     = Only x
    mappend (Only x) (Only y) = Only (x <> y)

newtype First' a =  
    First' { getFirst' :: Optional a }
    deriving (Eq, Show)

instance Monoid a => Monoid (First' a) where
    mempty  = First' {getFirst' = Nada}
    mappend x y = First' {getFirst' = (getFirst' x) <> (getFirst' y)} 
