import System.IO

f :: [String] -> a
f = undefined

main :: IO ()
main = openFile "n.i.txt" ReadMode >>= \handle -> 
       hGetContents handle >>= \contents ->
       print (f (lines contents)) >>
       hClose handle