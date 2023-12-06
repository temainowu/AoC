import System.IO

main :: IO ()
main = openFile "n.i.txt" ReadMode >>= \handle -> 
       hGetContents handle >>= \contents ->
       print (words contents) >>
       hClose handle