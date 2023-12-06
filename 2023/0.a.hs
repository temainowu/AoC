import System.IO
import Data.Char

main :: IO ()
main = openFile "0.i.txt" ReadMode >>= \handle -> 
       hGetContents handle >>= \contents ->
       print (sum (map f (words contents))) >>
       hClose handle

f :: String -> Int
f = (\xs -> toInt (head xs) * 10 + toInt (last xs)) . filter isDigit

toInt :: Char -> Int
toInt x | isDigit x = ord x - ord '0'