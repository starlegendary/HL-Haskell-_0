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