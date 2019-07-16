module Quiz4
where

import Data.Char
import Data.List
import Test.QuickCheck hiding (NonEmptyList)
import Test.QuickCheck.Modifiers hiding (NonEmptyList)
import Test.QuickCheck.Function

-- functor law

functor_prop1 :: Eq a => NonEmptyList a -> Bool
functor_prop1 = \x -> (fmap id x == id x)
functor_prop2 :: Eq a => Fun a a -> Fun a a -> NonEmptyList a -> Bool
functor_prop2 (Fn f) (Fn g) = \x -> ( 
          ((fmap f) . (fmap g)) x 
          == fmap (f . g) x
          )


-- Q2
data NonEmptyList a = One a | Cons a (NonEmptyList a) deriving (Eq)

instance Functor NonEmptyList where
  fmap f (One x) = One (f x)
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  


-- Q5
pair'     fa fb = pure fa <*> pure fb
pair''    fa fb = pure (,) <*> pure fa <*> pure fb
pair'''   fa fb = pure (,) <*> fa <*> fb  -- right
pair''''  fa fb = fmap (,) fa <*> fb  -- right


-- Q7
-- choice 3
s :: Monad m => [m a] -> m [a]
s [] = return []
s (a:as) = do
  x <- a
  xs <- s as
  pure (x : xs)
  

-- Q8
m :: Monad m => (a -> m b) -> [a] -> m [b]
m f = s . map f

f a
  | a > 0 = Just a
  | otherwise = Nothing