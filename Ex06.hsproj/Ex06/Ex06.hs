{-# LANGUAGE GADTs #-}

module Ex06 where

-- Datatype of formulas
-- --------------------

data Formula ts where
  Body   :: Term Bool                     -> Formula ()
  Exists :: Show a 
         => [a] -> (Term a -> Formula as) -> Formula (a, as)

data Term t where
  Name    :: String -> Term t    -- to facilitate pretty printing only. 
                                 -- don't use this in actual formulae.

  Con     :: t -> Term t -- Constant values

  -- Logical operators
  And     :: Term Bool -> Term Bool -> Term Bool
  Or      :: Term Bool -> Term Bool -> Term Bool

  -- Comparison operators
  Smaller :: Term Int  -> Term Int  -> Term Bool

  -- Arithmetic operators
  Plus    :: Term Int  -> Term Int  -> Term Int


-- Pretty printing formulas
-- ------------------------

instance Show t => Show (Term t) where
  show (Con v)       = show v
  show (And p q)     = "(" ++ show p ++ " && " ++ show q ++ ")"
  show (Or p q)      = "(" ++ show p ++ " || " ++ show q ++ ")"
  show (Smaller n m) = "(" ++ show n ++ " < "  ++ show m ++ ")"
  show (Plus n m)    = "(" ++ show n ++ " + "  ++ show m ++ ")"
  show (Name name)   = name

instance Show (Formula ts) where
  show = show' ['x' : show i | i <- [0..]]
    where
      show' :: [String] -> Formula ts' -> String
      show' ns     (Body body)   = show body
      show' (n:ns) (Exists vs p) = "exists " ++ n ++ "::" ++ show vs ++ ". " ++ show' ns (p (Name n))


-- Example formulas
-- ----------------

ex1 :: Formula ()
ex1 = Body (Con True)

ex2 :: Formula (Int, ())
ex2 = Exists [1..10] $ \n ->
        Body $ n `Smaller` (n `Plus` Con 1)

ex3 :: Formula (Bool, (Int, ()))
ex3 = Exists [False, True] $ \p -> 
      Exists [0..2] $ \n -> 
        Body $ p `Or` (Con 0 `Smaller` n)
        
-- my own tests
ex4 :: Formula (Bool, (Int, ()))
ex4 = Exists [False] $ \p -> 
      Exists [0] $ \n -> 
        Body $ p `Or` (Con 0 `Smaller` n)

ex5 :: Formula (Int, ())     
ex5 = Exists [-5..10] $ \n ->
        Body $ ((n `Plus` Con 1) `Smaller` n) `Or` (Con 0 `Smaller` n)

ex6 :: Formula (Int, (Int, ()))  
ex6 = Exists [1..5] $ \p ->
      Exists [-1..5] $ \n ->
        Body $ (p `Plus` n) `Smaller` p `And` (Con 1 `Smaller` p)   

-- Evaluating terms
-- ----------------
eval :: Term t -> t
eval p = case p of
  Con t -> t
  And t1 t2 -> eval t1 && eval t2
  Or  t1 t2 -> eval t1 || eval t2
  Smaller t1 t2 -> eval t1 < eval t2
  Plus t1 t2 -> eval t1 + eval t2
  Name _ -> error "eval: Name"  
-- the Name constructor is not relevant for evaluation
-- just throw an error if it is encountered:


-- Checking formulas
-- -----------------

satisfiable :: Formula ts -> Bool
satisfiable f = case f of
  Body t -> eval t
  Exists [] ttoF -> False
  Exists (x:xs) ttoF -> satisfiable (ttoF (Con x)) 
                        || satisfiable (Exists xs ttoF)


-- Enumerating solutions of formulae
-- ---------------------------------

solutions :: Formula ts -> [ts]
solutions f = case f of
  Body t -> [()]
  Exists ls ttoF -> [(a, b) | a <- ls,
                              b <- solutions (ttoF (Con a)),
                              satisfiable (ttoF (Con a))]
         

