{-# LANGUAGE UndecidableInstances #-}

module Lista4 where

-- Zad 1

type FlatGraph a = [(a, [a])]

data Node a = Node { lbl :: a, ns :: [Node a] }

instance (Show a) => Show (Node a) where  
  show :: Node a -> String 
  show (Node x xs) = show x ++ "->" ++ show (map lbl xs) 

type Graph a = [Node a]

g2 :: Graph Int
g2 =  let n1 = Node 1 [n2]
          n2 = Node 2 [n3]
          n3 = Node 3 [n1]
      in [n1, n2, n3]

flattenGraph :: (Eq a) => Graph a -> FlatGraph a
flattenGraph = map $ \(Node v e) -> (v, map lbl e)

makeGraph :: (Eq a) => FlatGraph a -> Graph a
makeGraph xs = undefined

-- Zad 2 ✅

newtype CPS o a = CPS { run :: (a -> o) -> o }

instance Functor (CPS o) where
  fmap :: (a -> b) -> CPS o a -> CPS o b
  fmap f (CPS g) = CPS (\h -> g (h . f))

-- Przyklad uzycia:

productCPS :: (Eq a, Num a) => [a] -> CPS o a
productCPS []     = CPS ($ 1)
productCPS (0:_)  = CPS ($ 0)
productCPS (x:xs) = CPS $ \k -> productCPS xs `run` (k . (* x))

productWithExit :: (Eq a, Num a) => [a] -> a
productWithExit xs = productCPS xs `run` id

showProductWithExit :: (Show a, Eq a, Num a) => [a] -> String
showProductWithExit xs = fmap show (productCPS xs) `run` id

-- Zad 3 ✅

class CoFunctor f where
  cofmap :: (a -> b) -> f b -> f a

newtype Predicate a = Predicate (a -> Bool)

instance CoFunctor Predicate where
  cofmap :: (a -> b) -> Predicate b -> Predicate a 
  cofmap f (Predicate p) = Predicate (p . f) 

-- Przyklad uzycia: 
  
filterPred :: Predicate a -> [a] -> [a]
filterPred (Predicate p) = filter p

hasThreeChars :: Predicate String
hasThreeChars = Predicate (\x -> length x == 3)

filtered :: [Int]
filtered = filterPred pred [1,12,123,1234,12345,321,21] where
  pred = cofmap show hasThreeChars

ok :: Bool
ok = filtered == [123,321]

-- Zad 4 ✅

data Term sig x = Var x
                | Op (sig (Term sig x))
                
deriving instance (Show x, Show (sig (Term sig x))) => Show (Term sig x) 

instance (Functor sig) => Functor (Term sig) where
  fmap :: (a -> b) -> Term sig a -> Term sig b 
  fmap f (Var v) = Var (f v)
  fmap f (Op s) = Op $ fmap (fmap f) s

var :: x -> Term sig x -- pure
var = Var

subst :: Functor sig => Term sig x -> (x -> Term sig y) -> Term sig y -- >>=
subst (Var v) f = f v 
subst (Op s) f = Op (fmap (`subst` f) s)

foldTerm :: Functor sig => (sig a -> a) -> (x -> a) -> Term sig x -> a
foldTerm _ g (Var v) = g v 
foldTerm f g (Op s) = f $ fmap (foldTerm f g) s 

-- Przyklad uzycia:

data ArithSig a = Plus a a | Minus a a | Times a a
  deriving (Show, Functor)

arithTerm :: Term ArithSig Int
arithTerm = Op (Plus (Var 2) (Op (Minus (Var 5) (Var 3))))

arithVal :: Int
arithVal = foldTerm op id arithTerm where
  op (Plus x y)  = x + y
  op (Minus x y) = x - y
  op (Times x y) = x * y

checkArith :: Bool
checkArith = arithVal == 4

data BoolSig a = Top | Bot | Or a a | And a a | Neg a
  deriving (Show, Functor)

boolTerm :: Term BoolSig Bool
boolTerm = Op (And (Var True) (Op (Or (Op Top) (Op (Neg (Var False))))))

boolVal :: Bool
boolVal = foldTerm op id boolTerm where
  op Top       = True
  op Bot       = False
  op (And x y) = x && y
  op (Or x y)  = x || y
  op (Neg x)   = not x

checkBool :: Bool
checkBool = boolVal

-- Zad 5 ✅

class (Applicative f) => ApplicativeError f where
  err :: String -> f a

instance ApplicativeError Maybe where
  err _ = Nothing

instance ApplicativeError (Either String) where
  err = Left

-- Uwaga na zmiane nazwy konstruktora (clash z zadaniem 4.) !!! 
data Expr = EOp String Expr Expr
          | Val Int
  deriving(Show)

eval :: (ApplicativeError f) => Expr -> f Int
eval (Val n) = pure n 
eval (EOp "+" e e') = (+) <$> eval e <*> eval e'
eval (EOp op _ _) = err $ "Unknown operator: " ++ op

-- Zad 6 ✅ (za 2p...?)

data Interactive
  = Done Int
  | NeedMoreInfo { operator :: String
                 , continue :: (Int -> Int -> Int) -> Interactive }

instance Show Interactive where
  show (Done n)           = "Done: " ++ show n
  show (NeedMoreInfo s _) = "Need more info on " ++ s

-- liftA2 :: (a -> b -> c) -> f a -> f b -> f c
liftA2' :: (Int -> Int -> Int) -> Interactive -> Interactive -> Interactive 
liftA2' op (Done n) (Done n') = Done (n `op` n')
liftA2' op (NeedMoreInfo op' cont) right =
  NeedMoreInfo op' $ \def -> liftA2' op (cont def) right 
liftA2' op left (NeedMoreInfo op' cont) =
  NeedMoreInfo op' $ \def -> liftA2' op left (cont def) 

evalI :: Expr -> Interactive
evalI (Val n) = Done n 
evalI (EOp "+" e e') = liftA2' (+) (evalI e) (evalI e')
evalI (EOp op e e') = NeedMoreInfo op $ \f -> liftA2' f (evalI e) (evalI e')

-- Żeby coś mogło być funktorem aplikatywnym to musi być jedno-argumentowym konstruktorem typu, a Interactive takim nie jest...(?)

-- Przykladowe wyrazenie:

exI :: Expr
exI = EOp "+" (EOp "^&" (Val 2) (Val 10))
              (EOp "%%" (Val 4) (Val 3))              
