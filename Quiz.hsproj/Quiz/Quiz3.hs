module Quiz3
where
  
import Data.Char
import Data.List
import Test.QuickCheck
import Test.QuickCheck.Modifiers



-- Q1

merge :: (Ord a) => [a] -> [a] -> [a]
merge (x:xs) (y:ys) | x < y     = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys
merge xs [] = xs
merge [] ys = ys

sorted :: Ord a => [a] -> Bool
sorted (x1 : x2 : xs) = (x1 <= x2) && sorted (x2 : xs)
sorted _ = True

-- two ordered must get a new ordered
q1_prop_1 :: OrderedList Int -> OrderedList Int -> Bool
q1_prop_1 (Ordered xs) (Ordered ys) = sorted (merge xs ys)

-- the length do not change
q1_prop_2 :: [Int] -> [Int] -> Bool
q1_prop_2 xs ys = length (merge xs ys) == length xs + length ys

-- merge = sort 
q1_prop_3 :: OrderedList Int -> OrderedList Int -> Bool
q1_prop_3 (Ordered xs) (Ordered ys) = merge xs ys == sort (xs ++ ys)

q1_prop_4 :: (Int -> Int) -> [Int] -> [Int] -> Bool
q1_prop_4 f xs ys = sort (map f (merge xs ys))
              == sort (merge (map f xs) (map f ys))

q1_prop_5 :: (Int -> Bool) -> OrderedList Int -> OrderedList Int -> Bool
q1_prop_5 f (Ordered xs) (Ordered ys) = filter f (merge xs ys) 
                                  == merge (filter f xs) (filter f ys)
                                  

-- Q2

rev :: [Int] -> [Int]
rev (x:xs) = rev xs ++ [x]
rev []     = []

q2_prop_1 :: [Int] -> [Int] -> Bool
q2_prop_1 xs ys = rev (xs ++ ys) == rev ys ++ rev xs

q2_prop_2 :: [Int] ->  Bool
q2_prop_2 xs = length xs == length (rev xs)

q2_prop_3 :: [Int] -> Int -> Bool
q2_prop_3 xs x = count x xs == count x (rev xs)
  where
    count x xs = length (filter (== x) xs)
    




-- data invariants

type Graph = [[Bool]]

wellformed :: Graph -> Bool
wellformed g = (transpose g == g) 
          && (all (\x -> length x == length g) g)





newGraph :: Int -- number of nodes
         -> Graph
newGraph n = replicate n (replicate n False)

connected :: Graph -> (Int, Int) -> Bool
connected g (x, y) | x < length g && y < length g = (g !! x) !! y 
                   | otherwise                    = False

connect :: (Int, Int) -> Graph -> Graph
connect (x, y) = modify y (modify x (\_ -> True)) . modify x (modify y (\_ -> True))
  where
    modify :: Int -> (a -> a) -> [a] -> [a]
    modify 0 f (x:xs) = f x : xs
    modify n f (x:xs) = x : modify (n - 1) f xs
    modify n f []     = []



data Model = M Int [(Int, Int)]

newGraphA :: Int -> Model
newGraphA n = M n []

connectedA :: Model -> (Int, Int) -> Bool
connectedA (M n es) (x,y) = (x,y) `elem` es

connectA :: (Int, Int) -> Model -> Model
connectA (x, y) (M n es) 
  | x < n && y < n = M n ((x,y):(y,x):es)
  | otherwise      = M n es   
  


ref1 :: Graph -> Model -> Bool
ref1 g (M n es) = all (\(x,y) -> (g !! x) !! y) es
              && length g == n

-- must wrong
ref2 :: Graph -> Model -> Bool
ref2 g (M n es) = all 
                  (\(x,y) -> ( ((x,y) `elem` es) == ((g !! x) !! y) ) )
                  [(x,y) | x <- [0..n-1], y <- [0..n-1]]
                  
ref3 :: Graph -> Model -> Bool
ref3 g (M n es) = length g == n
                  && any (\(x,y) -> ((x,y) `elem` es) == ((g !! x) !! y))
                     [(x,y) | x <- [0..n], y <- [0..n]] 
                  


ref4 :: Graph -> Model -> Bool
ref4 g (M n es) = length g == n
                  && all (\(x,y) -> (elem (x,y) es == ((g !! x) !! y)))
                     [(x,y) | x <- [0..n-1], y <- [0..n-1]] 
                     
                  
                    
-- Q6
toAbstract' :: Graph -> Model 
toAbstract' g = let n = length g 
                in M n $ filter (\(x,y) -> (g !! x ) !! y) 
                                [(x,y) | x <- [0..n-1], y <- [0..n-1]]

toAbstract'' :: Graph -> Model 
toAbstract'' g = let n = length g 
                in M n $ filter (\(x,y) -> (g !! x ) !! y) 
                                [(x,y) | x <- [0..n], y <- [0..n]]
                                
toConcrete :: Model -> Graph
toConcrete (M n es) 
   = map (\x -> map (\y -> (x,y) `elem` es) [0..n-1]) [0..n-1]