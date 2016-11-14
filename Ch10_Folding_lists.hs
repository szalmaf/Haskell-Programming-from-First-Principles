-- Show parantheses, left and right associations 
f = (\x y -> concat ["(",x,"+",y,")"])
foldl f "0" (map show [1..5])
foldr f "0" (map show [1..5])
