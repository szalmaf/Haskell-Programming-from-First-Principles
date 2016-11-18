-- 10.2

:t foldr
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

:t map
map :: (a -> b) -> [a] -> [b]


-- 10.3 Recursive patterns

sum :: [Integer] -> Integer
sum [] = 0 -- Base case
sum (x:xs) = x + sum xs -- Recursion

length :: [a] -> Integer
length [] = 0 -- Base case
length (_:xs) = 1 + length xs -- Recursion

concat :: [[a]] -> [a]
concat [] = [] -- Base case
concat (x:xs) = x ++ concat xs -- Recursion


-- 10.4 Fold right

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc [] = acc -- Base case
foldr f acc (x:xs) = f x (foldr f acc xs) -- Recursion

foldr f acc xs =
    case xs of
        []     -> acc -- Base case
        (x:xs) -> f x (foldr f acc xs) -- Recursion


-- 10.5 Fold left

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f acc [] = acc -- Base case
foldl f acc (x:xs) = foldl f (f acc x) xs -- Recursion

-- Show parantheses, left and right associations 
f = (\x y -> concat ["(",x,"+",y,")"])
foldl f "0" (map show [1..5])
foldr f "0" (map show [1..5])

foldr (+) 0 [1..5]
scanr (+) 0 [1..5]

foldl (+) 0 [1..5]
scanl (+) 0 [1..5]

foldr (:) [] [1,2,3] -- [1,2,3]
foldl (:) [] [1,2,3] -- NOT OK
foldl (flip (:)) [] [1..3] -- [3,2,1]

-- 10.6 How to write fold functions


-- Intermission: Excercises


-- 10.7 Folding and evaluation

-- 10.8 Sumamry

-- 10.9 Scans

-- 10.10 Chapter Exercises

