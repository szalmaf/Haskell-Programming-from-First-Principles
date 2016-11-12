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

myWords s = head : myWords tale
  where head = takeWhile (/=' ') s
        tail = drop 1 $ dropWhile (/= ' ') s

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
