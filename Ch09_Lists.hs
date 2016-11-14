Intermission: Exercises

1)
-- Break up String into a list os Strings (words) at the ' ' character

factorial 0 = 1; factorial n = n * factorial (n-1)

myWords "" = []; 
myWords x = takeWhile (/= ' ') x : 
       (myWords . 
       dropWhile (== ' ') . 
       dropWhile (/= ' ') 
       $ x)

myWords "" = []; 
myWords s = head : myWords tail
  where head = takeWhile (/=' ') s
        tail = dropWhile (== ' ') $ dropWhile (/= ' ') s

2)
-- Break up String at the '\n' character

myLines "" = []; 
myLines x = takeWhile (/='\n') x : 
       (myLines . 
       	dropWhile (=='\n') . 
       	dropWhile (/='\n') 
       	$ x)

3)
-- Parametrize on 

myLines ch "" = []; 
myLines ch x = takeWhile (/=ch) x : 
               ((myLines ch) . 
               	dropWhile (==ch) . 
               	dropWhile (/=ch) 
               	$ x)


xs = words "the brown dog was a goof"
[x | x <- xs, not (elem x ["the", "a"])]
filter (\x -> not (elem x ["the", "a"])) xs






