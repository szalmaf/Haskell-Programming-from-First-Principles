module Cipher where

import Data.Char

--Vigenere cipher

cipher = "ALLY"
message = "Meet at dawn"

cipherU = map toUpper cipher
messageU = map toUpper message

toNum x = ord x - ord 'A' -- x = [A..Z]
toChr x = chr $ x + ord 'A' -- x = [0..25]

-- f=(+) cipher; f=(-) decipher function
-- Need to deal with spaces!!!!!!
cipfn f c m = toChr $ mod(f (toNum m) (toNum c)) 26
res = zipWith (cipfn (+)) (concat $ repeat cipherU) messageU

