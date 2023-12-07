import System.IO

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f [] = []
splitBy f xs = takeWhile (not . f) xs : (splitBy f . drop 1 . dropWhile (not . f)) xs

intersect :: Eq a => [[a]] -> [a]
intersect [xs,ys] = filter (`elem` xs) ys 

points :: Int -> Int 
points 0 = 0
points 1 = 1
points n = 2 * points (n - 1)

f :: [String] -> Int
f = sum . map (points
              . length
              . intersect
              . map (filter (/= []))
              . splitBy (== "|")
              . splitBy (== ' ')
              . tail
              . dropWhile (/= ':'))

main :: IO ()
main = openFile "3.i.txt" ReadMode >>= \handle ->
       hGetContents handle >>= \contents ->
       print (f (lines contents)) >>
       hClose handle