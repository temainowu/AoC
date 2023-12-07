import System.IO
import Data.Char
import Control.Monad

(!) :: [[a]] -> (Int,Int) -> a
xss ! (i,j) = xss !! i !! j

data Things = O | X | N Int
    deriving (Eq, Show  )

data Gear = G | NG
    deriving (Eq, Show)

data ShowK = K [[(Things,Gear)]] | L [[Things]]

instance Show ShowK where
    show (K [])                  = []
    show (K ([]:xss))            = '\n'          : show (K xss)
    show (K (((O,NG):xs):xss))   = '.'           : show (K (xs:xss))
    show (K (((X,NG):xs):xss))   = '.'           : show (K (xs:xss))
    show (K (((X,G):xs):xss))    = '*'           : show (K (xs:xss))
    show (K (((N n,NG):xs):xss)) = '.'           : show (K (xs:xss))
    show (K (((N n,G):xs):xss))  = head (show n) : show (K (xs:xss))
    show (L [])             = []
    show (L ([]:xss))       = '\n'          : show (L xss)
    show (L ((O:xs):xss))   = '.'           : show (L (xs:xss))
    show (L ((X:xs):xss))   = '*'           : show (L (xs:xss))
    show (L ((N n:xs):xss)) = head (show n) : show (L (xs:xss))

isN :: Things -> Bool
isN (N _) = True
isN _     = False

thingToInt :: Things -> Int
thingToInt (N x) = x
thingToInt _ = 0

filterN :: ((Things,Gear) -> Bool) -> [(Things,Gear)] -> [(Things,Gear)]
filterN f [] = []
filterN f (x:xs) | f x = x : filterN f xs
                 | otherwise = (O,NG) : filterN f xs

toInt :: Char -> Int
toInt x | isDigit x = ord x - ord '0'

size :: String -> Int
size = length . takeWhile isDigit

represent :: String -> [Things]
represent [] = []
represent (x:xs) | isDigit x = N (toInt x * (10 ^ size xs)) : represent xs
                 | x == '*' = X : represent xs
                 | otherwise = O : represent xs

pad :: [[Things]] -> [[Things]]
pad xs = map ((O :) . (++ [O])) (row : xs ++ [row])
    where
        row = replicate (length (head xs)) O

pad' :: [[(Things,Gear)]] -> [[(Things,Gear)]]
pad' xs = map (((O,NG) :) . (++ [(O,NG)])) (row : xs ++ [row])
    where
        row = replicate (length (head xs)) (O,NG)

neighbours :: [[Things]] -> Int -> Int -> [Things]
neighbours xss i j = [ xss ! (i + x, j + y) | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0 ]

triples :: [a] -> [(a,a,a)]
triples (a:b:c:xs) = (a,b,c) : triples (b:c:xs)
triples xs = []

f :: [[Things]] -> [[Things]]
f = map ((map fst . filterN ((== G) . snd)) . g') . triples . pad' . map g . triples . pad

f_test :: [[Things]] -> ShowK
f_test = K . (map g' . triples . pad') . (map g . triples . pad)

removes :: [a] -> [[a]]
removes [] = []
removes (x:xs) = xs : map (x :) (removes xs)

two :: (a -> Bool) -> [a] -> Bool
two f xs = all (any f) (removes xs) && not (all (any f) (concatMap removes (removes xs)))

one :: (a -> Bool) -> [a] -> Bool
one f xs = any f xs && not (all (any f) (removes xs))

g :: ([Things],[Things],[Things]) -> [(Things,Gear)]
g (x0:x1:x2:xs,y0:y1:y2:ys,z0:z1:z2:zs) | y1 == X && (
                                          (isN x0 && (one isN [x2,y0,y2,z0,z1,z2]    ||  (any isN [z0,z1,z2] && not (isN z0 && isN z2 && not (isN z1))))) ||
                                          (isN x1 && (one isN [y0,y2,z0,z1,z2]       ||  (any isN [z0,z1,z2] && not (isN z0 && isN z2 && not (isN z1))))) ||
                                          (isN x2 && (one isN [x0,y0,y2,z0,z1,z2]    ||  (any isN [z0,z1,z2] && not (isN z0 && isN z2 && not (isN z1))))) ||
                                          (isN y0 && (one isN [x0,x1,x2,y2,z0,z1,z2] || ((any isN [x0,x1,x2] && not (isN x0 && isN x2 && not (isN x1))) /= (any isN [z0,z1,z2] && not (isN z0 && isN z2 && not (isN z1)))))) ||
                                          (isN y2 && (one isN [x0,x1,x2,y0,z0,z1,z2] || ((any isN [x0,x1,x2] && not (isN x0 && isN x2 && not (isN x1))) /= (any isN [z0,z1,z2] && not (isN z0 && isN z2 && not (isN z1)))))) ||
                                          (isN z0 && (one isN [x0,x1,x2,y0,y2,z2]    ||  (any isN [x0,x1,x2] && not (isN x0 && isN x2 && not (isN x1))))) ||
                                          (isN z1 && (one isN [x0,x1,x2,y0,y2]       ||  (any isN [x0,x1,x2] && not (isN x0 && isN x2 && not (isN x1))))) ||
                                          (isN z2 && (one isN [x0,x1,x2,y0,y2,z0]    ||  (any isN [x0,x1,x2] && not (isN x0 && isN x2 && not (isN x1)))))
                                                            ) = (X, G) : next y1
                                        | otherwise = (y1, NG) : next y1
    where
        next a = g (x1:x2:xs,a:y2:ys,z1:z2:zs)
g (_,y0:y1:ys,_) = [(y1,NG)]
g ([O],[O],[O]) = []

g' :: ([(Things,Gear)],[(Things,Gear)],[(Things,Gear)]) -> [(Things,Gear)]
g' (x0:x1:x2:xs,y0:y1:y2:ys,z0:z1:z2:zs) | isN (fst y1) && ((X,G) `elem` [x0,x1,x2,y0,y2,z0,z1,z2] || snd (head (next y1)) == G) = (fst y1, G) : next (X,G)
                                         | otherwise = y1 : next y1
    where
        next a = g' (x1:x2:xs,a:y2:ys,z1:z2:zs)
g' (_,y0:y1:ys,_) = [y1]
g' ([],[],[]) = []

getNums :: [[Things]] -> [((Int,Int),Int)]
getNums xss = zip coords (map (sum . map thingToInt . takeWhile isN . \(i,j) -> drop i (xss !! j)) coords)
    where coords = [ (i,j) | i <- [0..length xss - 1], j <- [0..length (head xss) - 1], isN (xss ! (i,j)) && not (isN (xss ! (i, j-1))) ]

pairNumsAroundGears :: [[Things]] -> [(Int,Int)]
pairNumsAroundGears xss = undefined

main :: IO [((Int,Int),Int)]
main = openFile "2.i.txt" ReadMode >>= (hGetContents >=>
     (return . (getNums . f . map represent . lines)))