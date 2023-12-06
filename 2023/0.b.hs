import System.IO
import Data.Char

fix :: String -> String
fix [] = []
fix xs@('z':'e':'r':'o':_)     = '0' : fix (tail xs)
fix xs@('o':'n':'e':_)         = '1' : fix (tail xs)
fix xs@('t':'w':'o':_)         = '2' : fix (tail xs)
fix xs@('t':'h':'r':'e':'e':_) = '3' : fix (tail xs)
fix xs@('f':'o':'u':'r':_)     = '4' : fix (tail xs)
fix xs@('f':'i':'v':'e':_)     = '5' : fix (tail xs)
fix xs@('s':'i':'x':_)         = '6' : fix (tail xs)
fix xs@('s':'e':'v':'e':'n':_) = '7' : fix (tail xs)
fix xs@('e':'i':'g':'h':'t':_) = '8' : fix (tail xs)
fix xs@('n':'i':'n':'e':_)     = '9' : fix (tail xs)
fix (x:xs) = x : fix xs

toInt :: Char -> Int
toInt x | isDigit x = ord x - ord '0'

f :: String -> Int
f = (\xs -> toInt (head xs) * 10 + toInt (last xs)) . filter isDigit

main :: IO ()
main = openFile "0.i.txt" ReadMode >>= \handle -> 
       hGetContents handle >>= \contents ->
       print (sum (map (f . fix) (words contents))) >>
       hClose handle
