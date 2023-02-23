-- chapter 1 ----------------------------------------------------------------
doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 then x else x * 2
doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

rightTriangles = [(a, b, c) | c <- [1..10], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2, a+b+c == 24]

-- chapter 4 ----------------------------------------------------------------
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "error"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x : replicate' (n-1) x

take' :: Int -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : (zip' xs ys)

elm' :: (Eq a) => a -> [a] -> Bool
elm' a [] = False
elm' a  (x:xs)
    | a == x = True
    | otherwise = elm' a xs

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
    let gte = [a | a <- xs, a >= x]
        lt  = [a | a <- xs, a < x]
    in (quickSort lt) ++ [x] ++ (quickSort gte)

-- chapter 5 ----------------------------------------------------------------
zipWith' :: (a -> b-> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y


largestDivisible :: Integer
largestDivisible = head (filter (\x -> x `mod` 3829 == 0) [100000, 99999..])

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
    | even n = n : chain (n `div` 2)
    | otherwise = n : chain (3 * n + 1)

numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- 5.4 lambda
flipWithLambda :: (a -> b -> c) -> b -> a -> c
flipWithLambda f = \x y -> f y x

-- 5.5 fold
sum' :: (Num a) => [a] -> a
sum' xs = foldl (+) 0 xs

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

elm'' :: (Eq a) => a -> [a] -> Bool
elm'' y ys = foldr (\x acc -> if x == y then True else acc) False ys

maximum'' :: (Ord a) => [a] -> a
maximum'' = foldl1 max

reverse'' :: [a] -> [a]
-- reverse'' = foldl (\acc x -> x : acc) []
reverse'' = foldl (flip (:)) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

sqrtSums :: Int
-- sqrtSums = length (takeWhile (< 1000) (scanl1 (+) (map sqrt [1..]))) + 1
sqrtSums = (length . takeWhile (< 1000) $ scanl1 (+) $ map sqrt [1..]) + 1

-- refactor with $ .
before :: [Integer]
before = replicate 2 (product (map (*3) (zipWith max [1,2] [4,5])))
after :: [Integer]
after = replicate 2 . product . map (*3) $ zipWith max [1,2] [4,5]