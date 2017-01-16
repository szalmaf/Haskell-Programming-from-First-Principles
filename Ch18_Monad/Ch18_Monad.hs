import Control.Monad (join)

-- import Test.QuickCheck
-- import Test.QuickCheck.Checkers
-- import Test.QuickCheck.Classes

-- class Applicative m => Monad m where 
--     (>>=)  :: m a -> (a -> m b) -> m b -- "bind"
--     (>>)   :: m a -> m b -> m b        -- "do sequencing"
--     return :: a -> m a                 -- "pure in Applicative"

-- fmap :: Functor f     =>   (a -> b) -> f a        -> f b
-- <*>  :: Applicative f => f (a -> b) -> f a        -> f b
-- >=   :: Monad f       => f a        -> (a -> f b) -> f b


-- 
-- Functor -> Applicative -> Monad

--fmap' f xs = xs >>= return . f

-- fmap (+1) [1..3]
-- [1..3] >>= return . (+1)

-- The novel part of monad
bind' :: Monad m => (a -> m b) -> m a -> m b
bind' f xs = join $ fmap f xs -- where f is a special: (a -> m b) type function!!

-- Monad lifting functions
-- liftM2 (,) (Just 3) (Just 5)
-- zipWith (+) [3,4] [5,6]
-- [8,10]
-- liftA2 (+) [3,4] [5,6]
-- [8,9,9,10]
-- zipWith3 (,,) [1,2] [3] [5,6]
-- [(1,3,5)]
-- liftM3 (,,) [1,2] [3] [5,6]
-- [(1,3,5),(1,3,6),(2,3,5),(2,3,6)]

-- import Control.Monad (join)
-- join $ putStrLn <$> getLine  -- need join to make
-- 2 IOs work or putStrLn >>= getLine 

-- do syntax and monads
bindingAndSequencing :: IO ()
bindingAndSequencing = do
    putStrLn "name pls: "
    name <- getLine
    putStrLn ("y helo thar: " ++ name)
-- monad syntax
bindingAndSequencing' :: IO ()
bindingAndSequencing' =
    putStrLn "name pls:" >>
    getLine >>=
    \name -> putStrLn ("y helo thar: " ++ name)

-- do syntax
twoBinds :: IO ()
twoBinds = do
    putStrLn "name pls:"
    name <- getLine
    putStrLn "age pls:"
    age <- getLine
    putStrLn ("y helo thar: "
              ++ name ++ " who is: "
              ++ age ++ " years old.")
-- monad bind syntax
twoBinds' :: IO ()
twoBinds' =
    putStrLn "name pls:" >>
    getLine >>=
    \name ->
    putStrLn "age pls:" >>
    getLine >>=
    \age ->
    putStrLn ("y helo thar: "
              ++ name ++ " who is: "
              ++ age ++ " years.")


-- List monad examples
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x
        then [x*x, x*x]
        else [x*x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
    x <- xs
    if even x
        then [x*x, x*x]
        else []

twiceWhenEven'' :: [Integer] -> [Integer]
twiceWhenEven'' xs = do
    xs  >>= \x -> (if even x
                    then [x*x, x*x]
                    else [1]) 
        >>= \x -> [3*x]

-- Maybe monad examples

data Cow = Cow  {
                    name :: String
                    , age :: Int
                    , weight :: Int
                } deriving (Eq, Show)

noEmpty :: String -> Maybe String 
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int 
noNegative n    
        | n >= 0 = Just n
        | otherwise = Nothing

-- if Cow's name is Bess, must be under 500
weightCheck :: Cow -> Maybe Cow 
weightCheck c =
    let w = weight c 
        n = name c
    in if n == "Bess" && w > 499 
        then Nothing
        else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow 
mkSphericalCow name' age' weight' =
    case noEmpty name' of 
        Nothing -> Nothing 
        Just nammy ->
            case noNegative age' of 
                Nothing -> Nothing 
                Just agey ->
                    case noNegative weight' of 
                        Nothing -> Nothing
                        Just weighty ->
                            weightCheck (Cow nammy agey weighty)

mkSphericalCow' :: String -> Int -> Int -> Maybe Cow 
mkSphericalCow' name' age' weight' = do
    nammy <- noEmpty name'
    agey <- noNegative age'
    weighty <- noNegative weight' 
    weightCheck (Cow nammy agey weighty)

mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow 
mkSphericalCow'' name' age' weight' =
    noEmpty name' >>= \ nammy ->
        noNegative age' >>= \ agey ->
            noNegative weight' >>= \ weighty ->
                weightCheck (Cow nammy agey weighty)

f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n

g :: Integer -> Maybe Integer
g i =
    if even i
    then Just (i + 1)
    else Nothing

h :: Integer -> Maybe String
h i = Just ("10191" ++ show i)

doSomething' n = do
    a <- f n
    b <- g a
    c <- h b
    return (a, b, c)

-- Exercise: Implement Either Monad
data Sum a b =
      First a 
    | Second b
    deriving (Eq, Show)
instance Functor (Sum a) where
    fmap f (Second y) = Second (f y)
    fmap _ (First x)  = First x -- First 
instance Monoid a => Applicative (Sum a) where -- well, if I define a as monoid!
    pure y = Second y
    (First x1)  <*> (First x2) = First (mappend x1 x2)
    (Second y)  <*> (First x)  = First x
    (First x)   <*> (Second y) = First x
    (Second f)  <*> (Second y) = Second (f y)
instance Monoid a => Monad (Sum a) where
    return = pure
    (Second y) >>= f  = (f y) -- f is (a -> m(!!) b)
    (First x)  >>= f  = First x -- Is this needed for First?




-- Chapter exercises

-- 1.
data Nope a =
    NopeDotJpg
instance Functor Nope where
    fmap f NopeDotJpg = NopeDotJpg
instance Applicative Nope where
    pure _ = NopeDotJpg
    _ <*> _ = NopeDotJpg
instance Monad Nope where
    return = pure
    _ >>= _ = NopeDotJpg

-- 2.
data PhhhbbtttEither b a =
      Left' a
    | Right' b
instance Functor (PhhhbbtttEither b) where
    fmap f (Left' y)  = Left' (f y)
    fmap _ (Right' x) = Right' x 
instance Monoid b => Applicative (PhhhbbtttEither b) where
    pure y = Left' y
    Left' f   <*> Left' y   = Left' (f y)
    Left' y   <*> Right' x  = Right' x
    Right' x  <*> Left' y   = Right' x
    Right' x1 <*> Right' x2 = Right' (mappend x1 x2)
instance Monoid b => Monad (PhhhbbtttEither b) where
    return = pure
    Left' y  >>= f = f y
    Right' x >>= f = Right' x      

-- 3.
newtype Identity a = Identity a deriving (Eq, Ord, Show)
instance Functor Identity where
    fmap f (Identity x) = Identity (f x)
instance Applicative Identity where
    pure x = Identity x
    Identity f <*> Identity x = Identity (f x) 
instance Monad Identity where
    return = pure
    Identity x >>= f = f x

-- 4.
data List' a =
      Nil
    | Cons a (List' a)
instance Functor List' where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)
instance Applicative List' where
    pure x = Cons x Nil
    Nil <*> x   = Nil
    f <*> Nil   = Nil
    Cons f fs <*> Cons x xs = Cons (f x) (Cons f fs <*> xs) -- simpler solution than in Appl Chapter
instance Monad List' where
    return x = Cons x Nil
    (Cons x xs) >>= f = Cons x' (xs >>= f)
        where Cons x' Nil = f x

-- 1.
j :: Monad m => m (m a) -> m a
j x = x >>= id -- Crazy !!!

-- 2.
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- 3.
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f x y = (fmap f x >>= (\f -> fmap f y)) -- can flip be used on rhs here?
-- lhs of >>= puts f x partial fn(s) inside m to become m fp
-- while the rhs of >>= takes these partial fn(s) and apply them
-- to each b in m b to get m c
-- l2 (,) [1,2][3, 6]
-- [(1,3),(1,6),(2,3),(2,6)]
-- l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
-- l2 f x y = (fmap f x) <*> y

-- 4.
a :: Monad m => m a -> m (a -> b) -> m b
a xs fs = fs >>= (\f -> fmap f xs)

-- 5.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh x:xs f = (fmap f (return [x])) : (meh xs f)
meh (x:xs) f = (return x >>= f) : (meh xs f)
meh (x:xs) f = (return x >>= (\x -> (f x) : (meh xs f)))
meh (x:xs) f = (return x >>= (\x -> (f (x : (meh xs f)))))
meh (x:xs) f = (return x >>= (\x -> f x)) : (meh xs f); meh [] _ = []




