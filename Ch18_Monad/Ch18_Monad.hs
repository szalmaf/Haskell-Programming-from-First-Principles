-- class Applicative m => Monad m where 
--     (>>=)  :: m a -> (a -> m b) -> m b -- "bind"
--     (>>)   :: m a -> m b -> m b        -- "do sequencing"
--     return :: a -> m a                 -- "pure in Applicative"

-- fmap :: Functor f     =>   (a -> b) -> f a        -> f b
-- <*>  :: Applicative f => f (a -> b) -> f a        -> f b
-- >=   :: Monad f       => f a        -> (a -> f b) -> f b


-- 
-- Functor -> Applicative -> Monad

--fmap' f xs = xs >>= return .f

-- fmap (+1) [1..3]
-- [1..3] >>= return . (+1)

-- do syntax
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
    pure (a, b, c)

-- Exercise: Implement Either Monad
data Sum a b =
      First a 
    | Second b
    deriving (Eq, Show)
instance Functor (Sum a) where
    fmap f xs = id