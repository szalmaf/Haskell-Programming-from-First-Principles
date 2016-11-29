-- As-patterns
f :: Show a => (a, b) -> IO(a, b)
f t@(a, _) = do
    print a 
    return t
rf = f (1, "fast haskell")
--print fst rf

doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x : xs

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf xss@(x:xs) yss@(y:ys) 
    | x == y && xs == []  = True
    | x == y && ys == []  = False
    | x == y              = isSubsequenceOf xs ys
    | x /= y && ys == []  = False
    | x /= y              = isSubsequenceOf xss ys
-- Tests
--isSubsequenceOf "blah" "blahwoot"
--isSubsequenceOf "blah" "blawoot"
--isSubsequenceOf "blah" "wootblah"
--isSubsequenceOf "blah" "wboloath"
--isSubsequenceOf "blah" "wootbla"
