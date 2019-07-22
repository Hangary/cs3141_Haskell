module Quiz5
where
  
import Data.Char
import Control.Monad.State
import Test.QuickCheck

-- Quiz 5
data Direction = L | R
forward    :: IO ()
forward = return ()
obstructed :: IO Bool
obstructed = return True
turn       :: Direction -> IO ()
turn direction = return ()

robot = do
  sensed <- obstructed
  if sensed 
    then turn L
    else forward
  robot


-- Quiz 6
q6a = do x <- getLine
         putStrLn (filter isDigit x)
         q6a
         
-- Choices:        
    
q61 = getLine 
      >>= putStrLn . filter isDigit 
      >> q61
q62 = getLine 
      >>= \x -> putStrLn (filter isDigit x) 
      >>= \_ -> q62
-- choice 3 has type-error
-- choice 4 has type-error
-- choice 5 has type-error
q66 = do x <- getLine; 
         putStrLn . filter isDigit $ x; 
         q66
q67 = do getLine >>= \x -> putStrLn (filter isDigit x); 
         q67
         

-- Quiz 7
--leftPad :: Int -> State String ()
--leftPad l = while ((< l) . length) $ do
--              str <- get
--              put (' ':str)
              

-- Quiz 8
matching :: String -> Int -> Bool
matching []       n = (n == 0)
matching ('(':xs) n = matching xs (n+1)
matching (')':xs) n = n > 0 && matching xs (n-1)
matching (oth:xs) n = matching xs n

-- Choices
matching2 xs = snd (runState (go xs) 0) == 0
  where
    go [] = pure True
    go (x:xs) | x == '('  = modify (+1) >> go xs
              | x == ')'  = do n <- get
                               if n > 0 then put (n - 1) >> go xs
                                        else pure False
              | otherwise = go xs 

matching3 xs = fst (runState (go xs) 0)
  where
    go [] = pure True
    go (x:xs) | x == '('  = modify (+1) >> go xs
              | x == ')'  = do n <- get
                               if n > 0 then put (n - 1) >> go xs
                                        else pure False
              | otherwise = go xs 


matching5 xs = fst (runState (go xs) 0)
  where
    go [] = get >>= pure . (== 0)
    go (x:xs) | x == '('  = modify (+1) >> go xs
              | x == ')'  = do n <- get
                               if n > 0 then put (n - 1) >> go xs
                                        else pure False
              | otherwise = go xs 
              

test_matching = matching5

newtype BracketString = BracketString String deriving Show

instance Arbitrary BracketString where
    arbitrary = BracketString <$> listOf (elements "ABC123efg()")

getString :: BracketString -> String
getString (BracketString s) = s

prop_matching :: BracketString -> Bool
prop_matching s = (test_matching (getString s)) == (matching (getString s) 0)

