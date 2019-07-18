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

-- Q4
instance Applicative NonEmptyList where
  pure x = Cons x (pure x)
  (One f) <*> (One x) = One (f x)
  (One f) <*> (Cons x _) = One (f x)
  (Cons f _) <*> (One x) = One (f x)
  (Cons f fs) <*> (Cons x xs) = Cons (f x) (fs <*> xs)


appLaw1 :: NonEmptyList Int -> Bool
appLaw1 v = (pure id <*> v) == v
-- Law 2
appLaw2 :: Bool
appLaw2 = (pureF <*> pureX) == pureY
  where pureF = pure (\x -> x * 3) 
        pureX = pure 3 :: NonEmptyList Int
        pureY = pure 9 :: NonEmptyList Int
-- Law 3



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