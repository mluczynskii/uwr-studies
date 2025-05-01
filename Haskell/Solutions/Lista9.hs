{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}

module Lista9 where

import Data.Typeable

-- ---------
-- Zadanie 1
-- ---------

type Size = Int

data Tree a = Leaf | Node Size a (Tree a) (Tree a) deriving (Show)

pattern Node' x l r <- Node _ x l r where 
  Node' x l r = balance $ Node (size l + size r + 1) x l r 
pattern Leaf' = Leaf

height :: Tree a -> Int
height Leaf = 0
height (Node _ _ l r) = max (height l) (height r) + 1

size :: Tree a -> Size
size Leaf = 0
size (Node s _ _ _) = s

singleL :: Tree a -> Tree a
singleL (Node' a l (Node' b rl rr)) =
  let newl = Node' a l rl in Node' b newl rr

singleR :: Tree a -> Tree a
singleR (Node' b (Node' a ll lr) r) =
  let newr = Node' b lr r in Node' a ll newr

doubleL :: Tree a -> Tree a
doubleL (Node' a l (Node' c (Node' b rll rlr) rr)) =
  let newl = Node' a l rll
      newr = Node' c rlr rr
  in Node' b newl newr

doubleR :: Tree a -> Tree a
doubleR (Node' c (Node' a ll (Node' b lrl lrr)) r) =
  let newl = Node' a ll lrl
      newr = Node' c lrr r
  in Node' b newl newr

ratio :: Size
ratio = 5

balance :: Tree a -> Tree a
balance Leaf' = Leaf'
balance t@(Node' _ l@(~(Node' _ ll lr)) r@(~(Node' _ rl rr)))
  | size l + size r < 2     = t
  | size r > ratio * size l = (if size rl < size rr then singleL else doubleL) t
  | size l > ratio * size r = (if size lr < size ll then singleR else doubleR) t
  | otherwise               = t

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf' = Node' x Leaf Leaf
insert x t@(Node' v l r)
  | x < v     = Node' v (insert x l) r
  | x > v     = Node' v l (insert x r)
  | otherwise = t

testTree :: Tree Int 
testTree = insert 1 
  $ insert 10 
  $ insert 3 
  $ insert 2137 
  $ insert 420
  $ insert (-10)
  $ insert 0
  $ insert 11 Leaf'

-- ---------
-- Zadanie 2
-- ---------


-- ---------
-- Zadanie 3
-- ---------

class (Typeable a) => IsShape a where
  name :: a -> String
  area :: a -> Double
  type' :: a -> TypeRep
  type' = typeOf

newtype Circle = Circle { radius :: Double }

instance IsShape Circle where
  name _ = "Circle"
  area a = 3.14 * (radius a ^ 2)

data Rect = Rect { a :: Double, b :: Double }

instance IsShape Rect where
  name _ = "Rect"
  area r = a r * b r

data Shape = forall x. (IsShape x) => Shape x

instance IsShape Shape where
  name (Shape x) = name x
  area (Shape x) = area x
  type' (Shape x) = type' x

shapeFactory :: String -> Shape
shapeFactory "Circle" = Shape $ Circle 6
shapeFactory "Rect"   = Shape $ Rect 2 5

type family ShapeArg s where
  ShapeArg Circle = Double
  ShapeArg Rect   = (Double, Double)

data SShape s where
  SCircle :: SShape Circle
  SRect   :: SShape Rect

shapeFactory2 :: SShape s -> ShapeArg s -> Shape
shapeFactory2 SCircle r     = Shape $ Circle r
shapeFactory2 SRect   (a,b) = Shape $ Rect a b

sameShape :: (IsShape a, IsShape b) => a -> b -> Bool
sameShape a b = type' a == type' b 

-- ---------
-- Zadanie 4
-- ---------

-- ---------
-- Zadanie 5
-- ---------
