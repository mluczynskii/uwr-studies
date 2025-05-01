module Lista3 where

import Data.Foldable
import qualified Data.List as List
import qualified System.Random as Random

-- Zad 1

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show)

flipVals :: Tree a -> Tree a
flipVals t = res where (xs, ys, res) = aux xs [] t

aux :: [a] -> [a] -> Tree a -> ([a], [a], Tree a)
aux ~(x:xs) ys (Leaf val) = (xs, val : ys, Leaf x)
aux xs ys (Node l r) =
    let (xs', ys', t1) = aux xs ys l
        (_, ys'', t2) = aux xs' ys' r
    in (ys'', ys'', Node t1 t2) 

-- Zad 2

newtype DiffList a = DiffList { unDiffList :: [a] -> [a] }

fromDiffList :: DiffList a -> [a]
fromDiffList = ($ []) . unDiffList 

toDiffList :: [a] -> DiffList a
toDiffList = DiffList . (++)

diffSingleton :: a -> DiffList a
diffSingleton = DiffList . (:)

instance Semigroup (DiffList a) where
    (<>) :: DiffList a -> DiffList a -> DiffList a 
    (<>) (DiffList f) (DiffList g) = DiffList (g . f)

instance Monoid (DiffList a) where
    mempty :: DiffList a 
    mempty = DiffList id

instance Foldable Tree where
    foldMap :: Monoid m => (a -> m) -> Tree a -> m 
    foldMap f (Leaf val) = f val 
    foldMap f (Node left right) = foldMap f left <> foldMap f right

test :: Integer -> Tree Char 
test n = if n == 0 then Leaf '.' else Node (test $ n-1) (test $ n-1) 

-- Zad 3

data CoinTree a = CTLeaf a
                | CTNode (CoinTree a) (CoinTree a) deriving Show

data ProbTree a = PTLeaf a
                | PTNode Double (ProbTree a) (ProbTree a) deriving Show

-- ex. 2/3 = [1, 3, 5, ...] (= 1/2 + 1/8 + 1/32 + ...)
binaryStream :: Double -> [Int]
binaryStream p = aux p 0.5 1 where 
    aux 0.0 _ _ = [] 
    aux p acc n = if p >= acc 
        then n : aux (p-acc) (acc/2) (n+1)
        else aux p (acc/2) (n+1)   

toCoinTree :: ProbTree a -> CoinTree a
toCoinTree (PTLeaf val) = CTLeaf val 
toCoinTree (PTNode p l r) =
    let l' = toCoinTree l 
        r' = toCoinTree r 
        xs = binaryStream p 
    in aux xs 1 l' r' where      
        aux [] _ _ r' = r'
        aux (x:xs) n l' r' = if x == n
            then CTNode l' (aux xs (n+1) l' r')
            else CTNode r' (aux (x:xs) (n+1) l' r')

coinRun :: [Bool] -> CoinTree a -> a
coinRun _ (CTLeaf val) = val 
coinRun (x:xs) (CTNode left right) =
    if x then coinRun xs left else coinRun xs right  

randomCoinRun :: CoinTree a -> IO a
randomCoinRun t = do 
  gen <- Random.initStdGen
  let bitStream = List.unfoldr (Just . Random.uniform) gen
  return (coinRun bitStream t)

-- Zad 4

data Frac = Frac Integer Integer

normalize :: Frac -> Frac 
normalize (Frac p q) = 
    let x = gcd p q in Frac (p `div` x) (q `div` x)

instance Num Frac where
    (+) :: Frac -> Frac -> Frac 
    (+) (Frac p q) (Frac p' q') =
        let result = Frac (p * q' + p' * q) (q * q') in normalize result 
    (*) :: Frac -> Frac -> Frac 
    (*) (Frac p q) (Frac p' q') =
        let result = Frac (p * p') (q * q') in normalize result 
    abs :: Frac -> Frac 
    abs (Frac p q) = 
        let result = Frac (abs p) q in normalize result 
    signum :: Frac -> Frac 
    signum (Frac p _) = (fromInteger . signum) p 
    fromInteger :: Integer -> Frac 
    fromInteger n = Frac n 1
    negate :: Frac -> Frac 
    negate (Frac p q) = 
        let result = Frac (-p) q in normalize result 

-- Zad 5

data CReal = CReal { unCReal :: [Frac] }
  
instance Num CReal where
    (+) :: CReal -> CReal -> CReal 
    (+) (CReal xs) (CReal ys) = CReal (zipWith (+) xs ys)
    (*) :: CReal -> CReal -> CReal 
    (*) (CReal xs) (CReal ys) = CReal (zipWith (*) xs ys)
    abs :: CReal -> CReal 
    abs (CReal xs) = CReal (map negate xs)
    signum :: CReal -> CReal 
    signum (CReal xs) = CReal (map signum xs)
    fromInteger :: Integer -> CReal
    fromInteger n = CReal [fromInteger n - Frac 1 i | i <- [0..]]
    (-) :: CReal -> CReal -> CReal 
    (-) (CReal xs) (CReal ys) = CReal (zipWith (-) xs ys)

unfoldStream :: (seed -> (seed, val)) -> seed -> [val]
unfoldStream f s = 
    let (nseed, nval) = f s 
    in nval : unfoldStream f nseed 

-- https://en.wikipedia.org/wiki/Leibniz_formula_for_%CF%80
realPi :: CReal
realPi = let seed = (4, -1, 3) in 
    let f (acc, sign, n) = ((acc + sign * Frac 4 n, -sign, n+2), acc) in 
        CReal (unfoldStream f seed)

toDouble :: Frac -> Double
toDouble (Frac p q) = fromRational (fromIntegral p / fromIntegral q) :: Double

stepN :: Int -> CReal -> Double
stepN n (CReal xs) = toDouble (xs !! n)

-- ghci> stepN 20000 realPi
-- 3.141642651089887
