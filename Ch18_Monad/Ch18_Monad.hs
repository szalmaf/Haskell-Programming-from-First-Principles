import Control.Monad (join)

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
bind' f xs = join $ fmap f xs 

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
    xs >>= \x -> if even x
                    then [x*x, x*x]
                    else []

-- Maybe monad examples
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
-- instance Functor (Sum a) where
--     fmap f xs = 

-- instance Applicative (Sum a) where
--     pure  =
--     (<*>) =

-- instance Monad (Sum a) where
--     return = pure
--     (>>=) = 