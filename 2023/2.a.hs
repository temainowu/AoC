{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import System.IO
import Data.Char
import Control.Monad

data Things = O | X | N Int
    deriving (Eq, Show)

data PartN = P | NP
    deriving (Eq, Show)

newtype ShowK = K [[(Things,PartN)]]

instance Show ShowK where
    show (K []) = []
    show (K ([]:xss)) = '\n' : show (K xss)
    show (K (((O,NP):xs):xss)) = '.' : show (K (xs:xss))
    show (K (((X,NP):xs):xss)) = '#' : show (K (xs:xss))
    show (K (((N n,NP):xs):xss)) = 'a' : show (K (xs:xss))
    show (K (((N n,P):xs):xss)) = head (show n) : show (K (xs:xss))

isN :: Things -> Bool
isN (N _) = True
isN _     = False

thingToInt :: Things -> Int
thingToInt (N x) = x
thingToInt _ = 0

toInt :: Char -> Int
toInt x | isDigit x = ord x - ord '0'

size :: String -> Int
size = length . takeWhile isDigit

represent :: String -> [Things]
represent [] = []
represent (x:xs) | isDigit x = N (toInt x * (10 ^ size xs)) : represent xs
                 | x == '.'  = O : represent xs
                 | otherwise = X : represent xs

pad :: [[Things]] -> [[Things]]
pad xs = map ((O :) . (++ [O])) (row : xs ++ [row])
    where
        row = replicate (length (head xs)) O

neighbours :: [[Things]] -> Int -> Int -> [Things]
neighbours xss i j = [ xss !! (i + x) !! (j + y) | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0 ]

triples :: [a] -> [(a,a,a)]
triples (a:b:c:xs) = (a,b,c) : triples (b:c:xs)
triples xs = []

f :: [[Things]] -> Int
f = sum . map (thingToInt . fst) . concatMap (filter ((== P) . snd) . g) . triples

f_test :: [[Things]] -> ShowK
f_test = K . map g . triples

g :: ([Things],[Things],[Things]) -> [(Things,PartN)]
g (x0:x1:x2:xs,y0:y1:y2:ys,z0:z1:z2:zs) | isN y1 && (X `elem` [x0,x1,x2,y0,y2,z0,z1,z2] || snd (head (next y1)) == P) = (y1, P) : next X
                                        | otherwise = (y1, NP) : next y1
    where
        next a = g (x1:x2:xs,a:y2:ys,z1:z2:zs)
g (_,y0:y1:ys,_) = [(y1,NP)]
g ([O],[O],[O]) = []

main :: IO Int
main = openFile "2.i.txt" ReadMode >>= (hGetContents >=>
     (return . (f . pad . map represent . lines)))