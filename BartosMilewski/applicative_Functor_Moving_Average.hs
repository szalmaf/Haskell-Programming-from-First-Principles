-- https://bartoszmilewski.com/2014/11/12/formula-1-go-functional/

data Chan a = Chan [a]

instance Functor Chan where 
    fmap f (Chan xs) = Chan (map f xs)

data Pressure = Pascal Float
data Volume   = Meter3 Float
data Temp     = Kelvin Float

constR = 8.314472 -- J/(mol • K)

getT :: Float -> Pressure -> Volume -> Temp
getT n (Pascal p) (Meter3 v) = Kelvin (p * v / (n * constR))

chT :: Chan Pressure -> Chan Volume -> Chan Temp
chT chP chV = getT <$> pure 0.1 <*> chP <*> chV

instance Applicative Chan where
    pure x = Chan (repeat x)
    Chan fs <*> Chan xs = Chan (zipWith ($) fs xs)

delay :: Num a => Int -> [a] -> [a]
delay n lst = replicate n 0 ++ lst

deltas :: Num a => Int -> [a] -> [a]
deltas n lst = map (uncurry (-)) (zip lst (delay n lst))

slidingSums :: Num a => Int -> [a] -> [a]
slidingSums n lst = scanl1 (+) (deltas n lst)

movingAverage :: Fractional a => Int -> [a] -> [a]
movingAverage n list = map (/ (fromIntegral n)) (slidingSums n list)







