import System.IO

data Cube = R Int | B Int | G Int
    deriving (Show, Eq)
type Handful = [Cube]
type Game = [Handful]

data NormalHandful = RBG Int Int Int | X
    deriving (Eq, Show)

bag = RBG 12 14 13

addHand :: Cube -> NormalHandful -> NormalHandful
addHand (R n) (RBG a b c) = RBG (a + n) b c
addHand (B n) (RBG a b c) = RBG a (b + n) c
addHand (G n) (RBG a b c) = RBG a b (c + n)

filterHand :: (NormalHandful -> Bool) -> [NormalHandful] -> [NormalHandful]
filterHand f [] = []
filterHand f (x:xs) | f x = x : filterHand f xs
                    | otherwise = X : filterHand f xs

handToIndex :: [NormalHandful] -> [Int]
handToIndex xs = [ i | (i,x) <- zip [1..] xs, x /= X]

join :: NormalHandful -> NormalHandful -> NormalHandful
join (RBG a b c) (RBG a' b' c') = RBG (max a a') (max b b') (max c c')

less :: NormalHandful -> NormalHandful -> Bool
less (RBG a b c) (RBG a' b' c') = a <= a' && b <= b' && c <= c'

normaliseHandful :: Handful -> NormalHandful
normaliseHandful = foldr addHand (RBG 0 0 0)

normaliseGame :: Game -> NormalHandful
normaliseGame = foldr (join . normaliseHandful) (RBG 0 0 0)

power :: NormalHandful -> Int
power (RBG a b c) = a * b * c

stringToCube :: String -> Cube
stringToCube xs = case reverse xs of
    ('d':'e':'r':' ':xs)         -> R (head [ n | n <- [0..], reverse xs == show n])
    ('e':'u':'l':'b':' ':xs)     -> B (head [ n | n <- [0..], reverse xs == show n])
    ('n':'e':'e':'r':'g':' ':xs) -> G (head [ n | n <- [0..], reverse xs == show n]) -- (read (reverse xs) :: Int)

stringToHandful :: String -> Handful
stringToHandful [] = []
stringToHandful (_:_:xs) = (\(xs,ys) -> stringToCube xs : stringToHandful ys) (break (== ',') xs)

stringToGame :: String -> Game
stringToGame [] = []
stringToGame (_:xs) = (\(xs,ys) -> stringToHandful (',':xs) : stringToGame ys) (break (== ';') xs)

dropGameNo :: String -> String
dropGameNo = (';':) . tail . dropWhile (/= ':')

main :: IO ()
main = openFile "1.i.txt" ReadMode >>= \handle ->
       hGetContents handle >>= \contents ->
       print (sum (map (power . normaliseGame . stringToGame . dropGameNo) (lines contents))) >>
       hClose handle