module Lista6 where

import Control.Monad (ap, guard)

newtype State s a =
  State { runState :: s -> (a, s) }
 deriving(Functor)

instance Applicative (State s) where
  pure a = State (a, )
  (<*>) = ap

instance Monad (State s) where
  State f >>= g = State $ \s ->
    let (a, s') = f s
    in  runState (g a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ const ((), s)

evalState :: State s a -> s -> a
evalState m = fst . runState m

-- ======
-- ZAD 1.
-- ======

gcdSub :: State (Int, Int) Int 
gcdSub = State aux where 
    aux (a, b) 
        | a > b = aux (a-b, b)
        | a < b = aux (a, b-a)
        | otherwise = (a, (a, b))

gcdSubOk :: Bool
gcdSubOk = evalState gcdSub (2*7*11*11*13, 5*7*7*13) == 7*13

-- ======
-- ZAD 2.
-- ======

doWhile :: (Monad m) => m Bool -> m a -> m a 
doWhile cond instr = do 
    result <- instr 
    test <- cond 
    if test 
        then doWhile cond instr 
        else pure result 

gcdMod :: State (Int, Int) Int 
gcdMod = let cond = State $ \s -> (snd s /= 0, s)
             instr = State $ \(a, b) -> (b, (b, a `mod` b))
            in doWhile cond instr 

gcdModOk :: Bool
gcdModOk = evalState gcdMod (2*7*11*11*13, 5*7*7*13) == 7*13

-- ======
-- ZAD 3.
-- ======

type Ident = String
type Op = String

data A  =  AOp Op A A
        |  AConst Int
        |  AVar Ident

data B  =  BCmp Op A A
        |  BOp Op B -- ...?
        |  BConst Bool
        |  BVar Ident

data C  =  Assign Ident A
        |  If B C C
        |  While B C
        |  Skip
        |  Cmp C C

data Memory = Memory {
    findInt :: Ident -> Int, 
    findBool :: Ident -> Bool
} 

evalA :: A -> State Memory Int
evalA (AVar id) = do 
    dict <- get 
    pure (findInt dict id) 
evalA (AConst n) = pure n 
evalA (AOp "+" e e') = do 
    n <- evalA e 
    n' <- evalA e' 
    pure (n + n')
evalA (AOp "-" e e') = do 
    n <- evalA e 
    n' <- evalA e' 
    pure (n - n')

evalB :: B -> State Memory Bool
evalB (BCmp ">" e e') = do 
    n <- evalA e 
    n' <- evalA e' 
    pure (n > n')
evalB (BCmp "<>" e e') = do 
    n <- evalA e 
    n' <- evalA e' 
    pure (n /= n')
evalB (BConst b) = pure b 
evalB (BVar id) = do 
    dict <- get 
    pure (findBool dict id)
evalB (BOp "!" e) = do 
    b <- evalB e 
    pure (not b)

evalC :: C -> State Memory ()
evalC (Assign id e) = do 
    (Memory dict dict') <- get 
    n <- evalA e
    let nmem = Memory (\id' -> if id' == id then n else dict id') dict' 
    put nmem
evalC (If cond e e') = do 
    b <- evalB cond 
    if b then evalC e else evalC e'
evalC (While cond e) = do 
    b <- evalB cond 
    if not b 
        then pure () 
        else do 
            evalC e 
            evalC (While cond e)
evalC Skip = pure ()
evalC (Cmp e e') = do 
    evalC e
    evalC e'

run :: C -> Memory -> Int
run e mem = let mem' = snd $ runState (evalC e) mem in 
    findInt mem' "result"

memZero :: Memory
memZero = Memory (const 0) (const False)

cGcd :: C 
cGcd = While (BCmp "<>" (AVar "result") (AVar "b"))
             (If (BCmp ">" (AVar "result") (AVar "b"))
                 (Assign "result" (AOp "-" (AVar "result") (AVar "b")))
                 (Assign "b" (AOp "-" (AVar "b") (AVar "result"))))

test :: Int -> Int -> Int 
test a b = run cGcd $ Memory (\id -> if id == "result" then a else b) (const False) 

-- ======
-- ZAD 4.
-- ======

type Solution = [Int]

queens :: Int -> [Solution]
queens n = queens' n n where 
    queens' n 1 = fmap pure [1..n]
    queens' n m = do 
        partial <- queens' n (m-1)
        choice <- [1..n]
        guard (extends choice partial)
        return (partial <> pure choice)

extends :: Int -> Solution -> Bool 
extends n xs = aux (length xs) xs n where 
    aux _ [] _ = True 
    aux dist (x:xs) n 
        | n == x = False
        | otherwise = abs (n-x) /= dist && aux (dist-1) xs n 

{- 
    n = 13 -> 73712 rozw.
    n = 14 -> 365596
    n = 15 -> 2279184
-}

-- ======
-- ZAD 5.
-- ======
(>>=) :: [a] -> (a -> [b]) -> [b]
xs >>= f = aux xs f [] where
    aux [] _ acc = acc 
    aux (x:xs) f acc = case f x of 
        [] -> []
        ys -> aux xs f (acc <> ys)

-- ======
-- ZAD 6.
-- ======
data BothFail a = None | Some [a]
    deriving (Functor)

instance (Show a) => Show (BothFail a) where 
    show None = "[]"
    show (Some xs) = show xs

instance Applicative BothFail where 
    pure x = Some [x]
    (<*>) = ap   

instance Monad BothFail where 
    None >>= _ = None 
    Some xs >>= f = concatMap' f xs [] where 
        concatMap' _ [] acc = Some acc 
        concatMap' f (x:xs) acc = case f x of 
            None -> None
            Some ys -> concatMap' f xs (acc <> ys)

-- Porażka lokalna
foo :: BothFail (Int, Char) 
foo = do 
    x <- Some [1..5]
    y <- if even x then Some [] else Some "ab"
    return (x, y)

-- Porażka globalna
bar :: BothFail (Int, Char)
bar = do 
    x <- Some [1..5]
    y <- if even x then None else Some "ab"
    return (x, y)

-- ghci> foo
-- [(1,'a'),(1,'b'),(3,'a'),(3,'b'),(5,'a'),(5,'b')]
-- ghci> bar
-- []