module Ex04 where

import Text.Read (readMaybe)

data Token = Number Int | Operator (Int -> Int -> Int)

parseToken :: String -> Maybe Token
parseToken "+" = Just (Operator (+))
parseToken "-" = Just (Operator (-))
parseToken "/" = Just (Operator div)
parseToken "*" = Just (Operator (*))
parseToken str = fmap Number (readMaybe str)

-- task 1
tokenise :: String -> Maybe [Token]
tokenise str = foldr (\a b -> (:) <$> a <*> b) (pure []) $ fmap parseToken $ words str


newtype Calc a = C ([Int] -> Maybe ([Int], a))

-- task 2
pop :: Calc Int
pop = C real_pop 
  where real_pop :: [Int] -> Maybe ([Int], Int)
        real_pop []     = Nothing
        real_pop (x:xs) = Just (xs, x)

push :: Int -> Calc ()
push i = C (real_push i)
  where real_push :: Int -> [Int] -> Maybe ([Int], ())
        real_push i stack = Just (i:stack, ())


instance Functor Calc where
  fmap f (C sa) = C $ \s ->
      case sa s of 
        Nothing      -> Nothing
        Just (s', a) -> Just (s', f a)

instance Applicative Calc where
  pure x = C (\s -> Just (s,x))
  C sf <*> C sx = C $ \s -> 
      case sf s of 
          Nothing     -> Nothing
          Just (s',f) -> case sx s' of
              Nothing      -> Nothing
              Just (s'',x) -> Just (s'', f x)

instance Monad Calc where
  return = pure
  C sa >>= f = C $ \s -> 
      case sa s of 
          Nothing     -> Nothing
          Just (s',a) -> unwrapCalc (f a) s'
    where unwrapCalc (C a) = a




-- task 3
evaluate :: [Token] -> Calc Int
evaluate ts = real_evaluate ts
  where real_evaluate :: [Token] -> Calc Int
        real_evaluate []      = pop
        real_evaluate (x:xs)  = case x of
          Number i   -> (push i) >> (real_evaluate xs)
          Operator f -> do
                      x <- pop
                      y <- pop
                      push (y `f` x) >> (real_evaluate xs)


calculate :: String -> Maybe Int
calculate s = case (tokenise s) of 
  Nothing -> Nothing
  Just t -> fmap snd $ unwrapCalc ((pure t) >>= evaluate) []
    where unwrapCalc (C a) = a

