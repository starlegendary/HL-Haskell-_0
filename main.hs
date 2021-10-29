import Data.Char

add :: Int -> Int -> Int
add x y = x+y

hhh :: [a] -> a
hhh (x:xs) = x

ttt :: (Num i, Ord i) => i -> [a] -> [a]  
ttt n _  
    | n <= 0   = []  
ttt _ []     = []  
ttt n (x:xs) = x : ttt (n-1) xs  

splitAt' :: [a] -> Int -> ([a],[a])
splitAt' ls n = (n `take` ls, n `drop` ls)

sign :: Int -> Int
sign x = fromEnum (x > 0) - fromEnum (x < 0) 

sign2 :: Int -> Int
sign2 x | x < 0 = -1
        | x > 0 = 1
        | otherwise = x

hasA :: [Char] -> Bool
hasA x = (hhh x) == 'a'

toTen :: Int -> (a -> Int)
toTen x = \_ -> x

pro :: [Int] -> Int
pro [] = 1
pro (x:xs) = x* pro(xs)

a = pro [1..b] where{b = 10};
d = a * 2

-- retur n number of odd integer
allodd :: Int -> [Int]
allodd n = map f [0..n-1] where f = \x -> 2*x + 1

(?) = \a -> (\d -> if a then fst d else snd d)
(#) = \b ->( \c -> (b,c)) 

toBool x = x == 1

sign3 :: Int -> Int
sign3 x = (x/= 0) ? ((x > 0) ? (1 # (-1)) # 0)


halve :: [a] -> ([a],[a])
halve ls = toBool ( (length ls) `mod` 2 ) ? ((ls,[]) # (n `take` ls, n `drop` ls)) where n = (length ls) `div` 2

-- list comprehensions

lsc0 = [(x,y) | x <- [1,2],y <- [3,4]]
lsc1 = [(x,y) | x <- [1,2],y <- [x..4]]


toint :: Char -> Int
toint c = ord (c) - ord ('a')
tochr :: Int -> Char
tochr n = chr (n + ord ('a'))

-- shift :: Int -> [Char] -> [Char]
-- shift n ls = map tochr (map (\x -> x+n) (map toint ls) )
shift :: Int -> Char -> Char
-- shift n c = tochr (n + toint c)
shift n = tochr. (\x -> x+n). toint
encode :: Int -> String -> String
encode n = map (shift n)
-- the z combinator
-- z f g x = \x -> f(g(x))(x)
zzz :: (b->a->c) -> (a-> b) -> (a -> c)
zzz f g = \x -> f (g x) x

rot :: Int -> [a] -> [a]
rot n ls = (drop n ls) ++ (take n ls)

percent:: Int -> Int -> Float
percent m n = (fromIntegral n / fromIntegral m) * 100


freq :: String -> [Int]
freq s = map (\x -> sum [1| c <- s, c == x] ) ['a'..'z']

allfreq :: String -> [Float]
allfreq s = ( map . percent . length)( s) (freq s)