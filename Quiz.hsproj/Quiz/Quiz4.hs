module Quiz4
where
  

-- Q2

data NonEmptyList a = One a | Cons a (NonEmptyList a)

instance Functor NonEmptyList where
  fmap f (One x) = One (f x)
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  
q2_prop1 = 