{-# LANGUAGE TypeFamilies #-}

module Lista10 where

import Data.Kind

-- --------------
-- Shape functors
-- --------------

class (Functor (Shape d)) => HasShape d where
  data Shape d :: Type -> Type
  wrap   :: Shape d d -> d
  unwrap :: d -> Shape d d

instance HasShape [a] where
  data Shape [a] x = ConsF a x | NilF
    deriving (Functor, Show)
  unwrap :: [a] -> Shape [a] [a]
  unwrap []     = NilF
  unwrap (x:xs) = ConsF x xs
  wrap :: Shape [a] [a] -> [a]
  wrap NilF         = []
  wrap (ConsF x xs) = x:xs

instance HasShape Int where
  data Shape Int x = S x | Z
    deriving (Functor)
  unwrap :: Int -> Shape Int Int
  unwrap 0 = Z
  unwrap n = S (n-1)
  wrap :: Shape Int Int -> Int
  wrap Z     = 0
  wrap (S n) = n + 1

-- ------------
-- Catamorphism
-- ------------

cata :: (HasShape d)
     => (Shape d a -> a) -> d -> a
cata f d = f             -- :: a
         $ fmap (cata f) -- :: Shape d a
         $ unwrap        -- :: Shape d d
         $ d             -- :: d

-- -----------
-- Anamorphism
-- -----------

ana :: (HasShape d)
    => (s -> Shape d s) -> s -> d
ana f s = wrap         -- :: d
        $ fmap (ana f) -- :: Shape d d
        $ f            -- :: Shape d s
        $ s            -- :: s

-- -----
-- Trees
-- -----

data Tree a = Node a (Tree a) (Tree a) | Leaf
  deriving (Show)

-- ==========
-- Zadanie 1. ✅
-- ==========

instance HasShape (Tree a) where
  data Shape (Tree a) x = NodeF a x x | LeafF
    deriving (Functor)
  unwrap :: Tree a -> Shape (Tree a) (Tree a)
  unwrap Leaf = LeafF 
  unwrap (Node e l r) = NodeF e l r
  wrap :: Shape (Tree a) (Tree a) -> Tree a 
  wrap LeafF = Leaf 
  wrap (NodeF e l r) = Node e l r

sumTreeAlg :: Shape (Tree Int) Int -> Int
sumTreeAlg LeafF = 0
sumTreeAlg (NodeF elem l r) = elem + l + r

prodTreeAlg :: Shape (Tree Int) Int -> Int
prodTreeAlg LeafF = 1
prodTreeAlg (NodeF elem l r) = elem * l * r

ex1 :: Tree Int
ex1 = Node 5 (Node 2 Leaf Leaf) (Node 10 Leaf Leaf)

-- Przykladowo:
-- λ> cata sumTreeAlg ex1
-- 17
-- λ> cata prodTreeAlg ex1
-- 100

-- ==========
-- Zadanie 2. ✅
-- ==========

sumProd :: Tree Int -> (Int, Int)
sumProd t = (cata sumTreeAlg t, cata prodTreeAlg t)

-- Przykladowo:
-- λ> sumProd ex1
-- (17,100)

-- fmap :: (a -> b) -> Shape d a -> Shape d b

parCata :: (HasShape d)
        => (Shape d a -> a)
        -> (Shape d b -> b)
        -> d
        -> (a, b)
parCata f g d = let shape = parCata f g <$> unwrap d in
  (f $ fmap fst shape, g $ fmap snd shape) 

parSumProd :: Tree Int -> (Int, Int)
parSumProd = parCata sumTreeAlg prodTreeAlg

zad2 :: Bool 
zad2 = parSumProd ex1 == sumProd ex1

-- ----------
-- Zadanie 3. ✅
-- ----------

minTreeAlg :: Shape (Tree Int) Int -> Int 
minTreeAlg (NodeF e l r) = min (min e l) r 
minTreeAlg LeafF = maxBound

repTreeAlg :: Int -> Shape (Tree Int) (Tree Int) -> Tree Int 
repTreeAlg n (NodeF _ l r) = Node n l r 
repTreeAlg _ LeafF = Leaf

repmin :: Tree Int -> Tree Int
repmin t = let (m, nt) = parCata minTreeAlg (repTreeAlg m) t in nt

zad3 :: Tree Int 
zad3 = repmin ex1

-- ----------
-- Zadanie 4. ✅
-- ---------- 

data Frac = Frac Int Int
  deriving (Show)

sternBrocot :: Tree Frac
sternBrocot = ana foo (Frac 0 1, Frac 1 1, Frac 1 0) where 
  foo (l@(Frac a b), m@(Frac c d), r@(Frac e f)) = 
    NodeF m (l, Frac (a+c) (b+d), m) (m, Frac (c+e) (d+f), r) 

-- ----------
-- Zadanie 5. ✅
-- ----------

bubbleAlg :: (Ord a) => Shape [a] (Shape [a] [a]) -> Shape [a] [a]
bubbleAlg NilF = NilF 
bubbleAlg (ConsF a (ConsF b rest)) = 
  if a > b then ConsF b (a:rest) else ConsF a (b:rest)
bubbleAlg (ConsF a NilF) = ConsF a [] 

bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort = ana (cata bubbleAlg)

zad5 :: [Int]
zad5 = bubbleSort [1,5,10,-1,2,3]

-- ----------
-- Zadanie 6. ✅
-- ----------

hylo :: forall d s b. (HasShape d)
     => (s -> Shape d s)
     -> (Shape d b -> b)
     -> s
     -> b
hylo f g = g . fmap (hylo f g) . f

fact :: Integer -> Integer
fact = hylo downto prod where
  downto 0 = NilF
  downto n = ConsF n (n-1)
  prod NilF        = 1
  prod (ConsF n k) = n * k

qsort :: forall a. (Ord a) => [a] -> [a]
qsort = hylo @(Tree a) toTree flatten where
  toTree  [] = LeafF 
  toTree (x:xs) = NodeF x (filter (<= x) xs) (filter (> x) xs)
  flatten LeafF = []
  flatten (NodeF a l r) = l ++ [a] ++ r

zad6 :: [Int]
zad6 = qsort [-1, 10, -50, 2, 5, 1, 20, 18, -2137]
