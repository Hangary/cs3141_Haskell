module Quiz1
where
  
-- Q1
rot13 :: String -> String
rot13 = map $ \x -> 
          case lookup x table of
            Just y  -> y 
            Nothing -> x
  where
    table = table' 'A' 'Z' ++ table' 'a' 'z'
    table' a z = zip [a..z] (drop 13 (cycle [a..z]))
    
test1 :: String -> Bool
test1 x = length x == length (rot13 x)

test1 :: String -> Bool
test2 = rot13 (map toUpper x) == map toUpper (rot13 x)

test3 = rot13 (map f x) == map f (rot13 x)

test4 = all (not . isAlpha) x ==> rot13 x == x

test5 = rot13 (a ++ b) == rot13 a ++ rot13 b

test6 = not (null x) ==> ord (head x) + 13 == ord (head (rot13 x)) 

test7 = rot13 (rot13 x) == x