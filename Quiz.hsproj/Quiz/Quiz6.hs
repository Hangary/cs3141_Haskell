{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Quiz6
where
  
import Data.Char
import Control.Monad.State
import Test.QuickCheck

-- Linear Algebra

data Size = OneD  | TwoD | ThreeD
newtype Vector (s :: Size) a = V [a] deriving (Show)

vec1D :: a -> Vector OneD a
vec1D a = V [a]

vec2D :: (a, a) -> Vector TwoD a
vec2D (a,b) = V [a,b]

vec3D :: (a, a, a) -> Vector ThreeD a
vec3D (a,b,c) = V [a,b,c]

-- q3
addV :: (Num n) => Vector a n -> Vector a n -> Vector a n
addV (V xs) (V ys) = V (zipWith (+) xs ys)

-- q4
type Matrix r c a = Vector r (Vector c a) 


-- q5: [3]
data IsEmpty = Empty | NotEmpty
data Tree (t :: IsEmpty) a where
  Leaf :: Tree Empty a
  Branch :: a -> Tree t1 a -> Tree t2 a -> Tree NotEmpty a
  
root :: Tree NotEmpty a -> a
root (Branch v _ _) = v

-- q6
data Exp a where
  Plus :: Exp Int -> Exp Int -> Exp Int
  Const :: (Show a) => a -> Exp a 
  LessOrEq :: Exp Int -> Exp Int -> Exp Bool
  Not :: Exp Bool -> Exp Bool
  And :: Exp Bool -> Exp Bool -> Exp Bool
  
-- q7: [4] not sure!!! TODO
evalArith :: Exp a -> a
evalArith (Plus a b) = evalArith a + evalArith b
evalArith (Const a) = a  


-- q8: [2,3] not sure!!! TODO
data Id a b where
  Refl :: Id x x
  
f :: Num a => Id a Int -> a
f = \_ -> 0