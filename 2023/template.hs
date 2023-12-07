import System.IO

f :: String -> IO ()
f = undefined

main :: IO ()
main = openFile "n.i.txt" ReadMode >>= \handle -> 
       hGetContents handle >>= \contents ->
       f contents >>
       hClose handle