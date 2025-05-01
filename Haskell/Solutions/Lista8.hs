-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE TypeApplications #-}

module Lista8 where
import Data.List (singleton)

-- ==========
-- Zadanie 1.
-- ==========

data Pair a = a :< a

data Tree x = Node (Tree x) (Tree x) | Leaf x

data Term sig x = Var x
                | Op (sig (Term sig x))

class Term_ t sig | t -> sig where 
  var :: x -> t x 
  op :: sig x -> t x
  subst :: t x -> (x -> t y) -> t y
  foldTree :: (sig a -> a) -> (x -> a) -> t x -> a

instance Functor sig => Term_ (Term sig) sig where 
  var = Var 
  op = Op . fmap Var 
  subst (Var x) f = f x 
  subst (Op s) f = undefined  
  foldTree _ f (Var x) = f x 
  foldTree comb f (Op s) = 
    undefined

instance Term_ Tree Pair where 
  var = Leaf 
  op (x :< y) = Node (Leaf x) (Leaf y)  
  subst (Leaf x) f = f x 
  subst (Node t1 t2) f = Node (subst t1 f) (subst t2 f) 
  foldTree _ f (Leaf x) = f x 
  foldTree comb f (Node t1 t2) = 
    comb (foldTree comb f t1 :< foldTree comb f t2)  

-- ==========
-- Zadanie 2. ✅
-- ==========

data Expr = Val Int
          | Add Expr Expr
 deriving (Show)

eval :: Expr -> Int
eval (Val n)   = n
eval (Add l r) = eval l + eval r

pprint :: Expr -> String
pprint (Val n) = show n
pprint (Add l r) = "(" ++ pprint l ++ " + " ++ pprint r ++ ")"

ex1 :: Expr
ex1 = Add (Add (Val 3) (Val 9)) (Val 10)

class ExprFT d where
  val :: Int -> d
  add :: d -> d -> d

instance ExprFT Int where 
  val = id 
  add = (+)

instance ExprFT String where 
  val = show 
  add lhs rhs = "(" ++ lhs ++ " + " ++ rhs ++ ")"

instance ExprFT Expr where 
  val = Val 
  add = Add

toFT :: ExprFT d => Expr -> d 
toFT (Val x) = val x 
toFT (Add lhs rhs) = add (toFT lhs) (toFT rhs)

evalFT :: ExprFT d => d -> d
evalFT = id

ex2 :: ExprFT d => d
ex2 = add (add (val 3) (val 9)) (val 10)

-- ==========
-- Zadanie 3. ✅
-- ==========

class ExprFT' a b | a -> b, b -> a where 
  val' :: Int -> a 
  add' :: a -> a -> a 
  eq :: a -> a -> b 
  neg :: b -> b  
  if' :: b -> a -> a -> a

instance ExprFT' Int Bool where 
  val' = id 
  add' = (+)
  eq = (==)
  neg = not 
  if' test t f = if test then t else f 

instance ExprFT' String String where 
  val' = show 
  add' lhs rhs = "(" ++ lhs ++ " + " ++ rhs ++ ")"
  eq x y = "(" ++ x ++ " == " ++ y ++ ")" 
  neg b = "!" ++ b
  if' test t f = "if " ++ test ++ " then " ++ t ++ " else " ++ f

evalA :: ExprFT' a b => a -> a 
evalA = id 

evalB :: ExprFT' a b => b -> b 
evalB = id 

ex3 :: ExprFT' a b => a
ex3 = if' (neg $ eq (val' 1) (val' 2)) (add' (val' 2) (val' 2)) (val' 2137)

ex4 :: ExprFT' a b => b 
ex4 = neg (eq (val' 1) (val' 2))

-- ==========
-- Zadanie 4.
-- ==========

-- ==========
-- Zadanie 5. ✅
-- ==========

class Sum a where
  sum :: Integer -> a

instance Sum Integer where 
  sum = id 

instance Sum b => Sum (Integer -> b) where 
  sum x = Lista8.sum . (x +) 

-- ==========
-- Zadanie 6. ✅
-- ==========

class Flatten a b where
  flatten :: a -> [b]

instance Flatten Char Char where 
  flatten = singleton

instance Flatten a b => Flatten [a] b where 
  flatten = concatMap flatten

instance (Flatten a b, Flatten a' b) => Flatten (a, a') b where 
  flatten (x, y) = flatten x ++ flatten y 

-- ==========
-- Zadanie 7. ✅
-- ==========  

data Nat = Zero | Succ Nat 

data RWList balance a where 
  Nil :: RWList Zero a
  White :: a -> RWList n a -> RWList (Succ n) a
  Red :: a -> RWList (Succ n) a -> RWList n a

-- ==========
-- Zadanie 8.
-- ==========
