import Data.Char
import Data.List
import Test.QuickCheck

-- Q1
rot13 :: String -> String
rot13 = map $ \x -> 
          case lookup x table of
            Just y  -> y 
            Nothing -> x
  where
    table = table' 'A' 'Z' ++ table' 'a' 'z'
    table' a z = zip [a..z] (drop 13 (cycle [a..z]))
    

test1 x = length x == length (rot13 x)
test2 x = rot13 (map toUpper x) == map toUpper (rot13 x)
test3 x = rot13 (map f x) == map f (rot13 x)
  where 
    f :: Char -> Char
    f x = chr $ ord x + 1
-- input: not . isAlpha
test4 x = rot13 x == x
test5 a b = rot13 (a ++ b) == rot13 a ++ rot13 b
-- input: not (null x)
test6 x = not (null x) ==> ord (head x) + 13 == ord (head (rot13 x)) 
test7 x = rot13 (rot13 x) == x



-- Q2
merge :: (Ord a) => [a] -> [a] -> [a]
merge (x:xs) (y:ys) | x < y     = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys
merge xs [] = xs
merge [] ys = ys


q2t1 a b = merge (sort a) (sort b) == sort (merge a b)
q2t2 a b = merge a b == sort (a ++ b)
q2t3 a b = length (merge a b) == length a + length b
q2t4 a b = merge (filter f a) (filter f b) == filter f (merge a b)
  where f = \x -> x > 4
q2t5 a b = merge (map f a) (map f b) == map f (merge a b)
  where f = \x -> -x
q2t6 a b = sort (merge a b) == sort (a ++ b)


-- Q3
toBinary :: Int -> String
toBinary 0 = ""
toBinary n = let (d,r) = n `divMod` 2
              in toBinary d 
                   ++ if r == 0 then "0"
                                else "1"

fromBinary :: String -> Int
fromBinary = fst . foldr eachChar (0,1)
  where
    eachChar '1' (sum, m) = (sum + m, m*2)
    eachChar _   (sum, m) = (sum    , m*2)
    

q3t1 i = i >= 0 ==> fromBinary (toBinary i) == i
q3t2 s = all (`elem` "01") s ==> toBinary (fromBinary s) == s
q3t3 s = (read s :: Int) >= fromBinary s
q3t4 i = i > 0 ==> length (toBinary i) >= length (show i)
q3t5 s = all (`elem` "01") s ==> fromBinary s == fromBinary ('0':s)


-- Q4
dedup :: (Eq a) => [a] -> [a]
dedup (x:y:xs) | x == y = dedup (y:xs)
               | otherwise = x : dedup (y:xs)
dedup xs = xs

sorted :: (Ord a) => [a] -> Bool
sorted (x:y:xs) = x <= y && sorted (y:xs)
sorted xs = True


q4t1 (Ordered xs) = sorted xs ==> sorted (dedup xs)
q4t2 (Ordered xs) = sorted xs ==> dedup xs == nub xs
q4t3 (Ordered xs) = sorted xs ==> dedup (dedup xs) == dedup xs
q4t4 (Ordered xs) (Ordered ys) = sorted xs && sorted ys ==> dedup xs ++ dedup ys == dedup (xs ++ ys)
q4t5 (Ordered xs) = sorted xs ==> length (dedup xs) < length xs
q4t6 x xs = (x `elem` xs) == (x `elem` dedup xs)



-- Q5
foo :: [a] -> (a -> b) -> [b]
foo [] f = []
foo (x:xs) f = f x : foo xs f


q5t1 :: [Int] -> Bool
q5t1 xs = foo xs id == xs 

q5t2 :: [Int] -> Bool
q5t2 xs = foo (foo xs f) g == foo xs (g . f)
  where f = \x -> x + 1
        g = \x -> x * 2


-- Q6
bar :: [Int] -> [Int]
bar xs = replicate (length xs) (maximum xs)

q6t1 :: [Int] -> Bool
q6t1 xs = bar (bar xs) == xs

q6t2 :: [Int] -> Bool
q6t2 xs = length xs == length (bar xs)

q6t3 :: [Int] -> Bool
q6t3 xs = bar (map f xs) == map f (bar xs)
  where f = \x -> x - 1
  

-- Q7
baz :: [Integer] -> Integer
baz xs = 0

q7t1 :: [Integer] -> [Integer] -> Bool
q7t1 xs ys = baz xs + baz ys == baz (xs ++ ys)

q7t2 :: [Integer] -> Bool
q7t2 xs = baz xs == baz (reverse xs) 

q7t3 :: Integer -> [Integer] -> Bool 
q7t3 x xs = baz (x:xs) - x == baz xs


-- Q8
fun :: [Integer] -> [Integer]
fun []       = []
fun [x]      = []
fun (x:y:xs) = (y-x):fun (y:xs)

nuf xs i = scanl (\v x -> v + x) i xs

q8t1 :: [Integer] -> Integer -> Bool
q8t1 xs x = nuf (fun (x:xs)) x == (x:xs)

q8t2 :: [Integer] -> Integer -> Bool
q8t2 xs x = fun (nuf xs x) == xs