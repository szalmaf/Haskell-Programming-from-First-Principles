import Data.List (elemIndex)


added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])

y0 :: Maybe Integer
y0 = lookup 4 $ zip [1,2,3] [4,5,6]
z0 :: Maybe Integer
z0 = lookup 2 $ zip [1,2,3] [4,5,6]
tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y0 <*> z0

x1 :: Maybe Int
x1 = elemIndex 4 [1,2,3,4,5]
y1 :: Maybe Int
y1 = elemIndex 4 [1,2,3,4,5]
max' :: Int -> Int -> Int
max' = max
maxed :: Maybe Int
maxed = max' <$> x1 <*> y1

xs2 = [1,2,3]
ys2 = [4,5,6]
x2 :: Maybe Integer
x2 = lookup 3 $ zip xs2 ys2
y2 :: Maybe Integer
y2 = lookup 2 $ zip xs2 ys2
summed :: Maybe Integer
summed = sum <$> ((,) <$> x2 <*> y2) -- probably something else is expected




main :: IO ()
main = do
  putStrLn "hello world"
