import Data.Monoid
import Test.QuickCheck

data Optional a =
      Nada
    | Only a
    deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where 
    mempty  = Nada
    mappend Nada Nada         = Nada
    mappend Nada (Only x)     = Only x
    mappend (Only x) Nada     = Only x
    mappend (Only x) (Only y) = Only (x <> y)

newtype First' a =  
    First' { getFirst' :: Optional a }
    deriving (Eq, Show)

instance Monoid a => Monoid (First' a) where
    mempty  = First' {getFirst' = Nada}
    mappend x y = First' {getFirst' = (getFirst' x) <> (getFirst' y)} 

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = frequency [(1,  return mempty), 
                           (10, fmap . fmap $ First' {getFirst' = Only arbitrary})]

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc x y z = x <> (y <> z) == (x <> y) <> z

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity x = (x <> mempty) == x

monoidRightIdentity :: (Eq a, Monoid a) => a -> Bool
monoidRightIdentity x = (mempty <> x) == x 



