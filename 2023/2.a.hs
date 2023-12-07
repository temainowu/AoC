import System.IO

main :: IO ()
main = openFile "2.i.txt" ReadMode >>= \handle -> 
       hGetContents handle >>= \contents ->
       print (words contents) >>
       hClose handle