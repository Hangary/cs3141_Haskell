{-# LANGUAGE GADTs, DataKinds, KindSignatures, TupleSections, PolyKinds, TypeOperators, TypeFamilies, PartialTypeSignatures #-}
module Hare where
import Control.Monad
import Control.Applicative 
import HareMonad 

-- task 1 and 2

data RE :: * -> * where 
  Empty :: RE ()
  Fail :: RE a
  Char :: [Char] -> RE Char
  Seq :: RE a -> RE b -> RE (a, b)
  Choose :: RE a -> RE a -> RE a
  Star :: RE a -> RE [a]
  Action :: (a -> b) -> RE a -> RE b

match :: (Alternative f, Monad f) => RE a -> Hare f a
match re = case re of
  Empty -> pure ()
  Fail -> failure
  Char cs -> do
              x <- readCharacter
              guard (x `elem` cs)
              pure x
  Seq a b -> (,) <$> match a <*> match b
  Choose a b -> match a <|> match b
  Star a -> (:) <$> match a <*> match (Star a)
            <|> pure []
  Action f r -> fmap f (match r)
            
matchAnywhere :: (Alternative f, Monad f) => RE a -> Hare f a
matchAnywhere re = match re <|> (readCharacter >> matchAnywhere re)

(=~) :: (Alternative f, Monad f) => String -> RE a -> f a 
(=~) = flip (hare . matchAnywhere)


-- task 3

infixr `cons`  
cons :: RE a -> RE [a] -> RE [a]
cons x xs = Action (\(l,ls) -> l:ls) (Seq x xs)

-- using Action, Seq and Star
plus :: RE a -> RE [a]
plus re = re `cons` (Star re)

-- using Char, cons, Empty and Action
string :: String -> RE String
string s = case s of 
  [] -> Action (\_ -> []) Empty
  x:xs -> Action (\(l,ls) -> l:ls) (Seq (Char [x]) (string xs))
  
-- using Action, Choose and Empty
choose :: [RE a] -> RE a
choose res = case res of
  [] -> Fail
  x:xs -> Choose x (choose xs)
  
-- bug
option :: RE a -> RE (Maybe a)
option re = Choose (Action (\x -> Just x) re) (Action (\_ -> Nothing) Empty)

        
rpt :: Int -> RE a -> RE [a]
rpt n re
  | n <= 0    = Action (\_ -> []) Empty
  | otherwise = re `cons` (rpt (n - 1) re)

rptRange :: (Int, Int) -> RE a -> RE [a]
rptRange (x,y) re = choose $ helper (x,y) re
  where helper (i,j) re
          | i > j  = []
          | i == j = [rpt i re]
          | i < j  = [rpt j re] ++ helper (i,j-1) re