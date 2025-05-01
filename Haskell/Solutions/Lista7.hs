module Lista7 where

import Control.Monad
import Data.Kind (Type)
import Data.Maybe (fromJust)
import Data.Monoid

-- State monad

newtype State s a
  = State {runState :: s -> (a, s)}
  deriving (Functor)

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  (<*>) = ap

instance Monad (State s) where
  State f >>= g = State $ \s ->
    let (a, s') = f s
     in runState (g a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

update :: (s -> s) -> State s ()
update f = get >>= put . f

-- Writer monad

newtype Writer w a = Writer {runWriter :: (w, a)}
  deriving (Functor, Show)

instance (Monoid w) => Applicative (Writer w) where
  pure a = Writer (mempty, a)
  (<*>) = ap

instance (Monoid w) => Monad (Writer w) where
  Writer (w, a) >>= f =
    Writer (w <> w', a')
    where
      Writer (w', a') = f a

tell :: w -> Writer w ()
tell w = Writer (w, ())

-- Tree

data Tree = Node Tree Tree | Leaf Int
  deriving (Show)

t1 :: Tree
t1 =
  Node
    (Node (Leaf 2) (Leaf 1))
    (Leaf 3)

-- ==========
-- ZADANIE 1. ✅
-- ==========

data Min a = Min a | Id -- a \sqcup {1}
  deriving (Show)

toMin :: a -> Min a
toMin = Min

fromMin :: Min a -> a
fromMin (Min x) = x

instance (Ord a) => Semigroup (Min a) where
  Id <> x = x
  x <> Id = x
  Min a <> Min b = Min $ min a b

instance (Ord a) => Monoid (Min a) where
  mempty = Id

treeMinW :: Tree -> Writer (Min Int) ()
treeMinW (Leaf n) = tell (toMin n)
treeMinW (Node l r) = treeMinW l >> treeMinW r

treeMin :: Tree -> Int
treeMin t = fromMin $ (fst . runWriter . treeMinW) t

-- ==========
-- ZADANIE 2. ✅
-- ==========

repmin :: Tree -> Tree
repmin t = t'
  where
    (Min n, t') = runWriter (aux n t)

aux :: Int -> Tree -> Writer (Min Int) Tree
aux n (Leaf m) = tell (toMin m) >> pure (Leaf n)
aux n (Node l r) = do
  l' <- aux n l
  r' <- aux n r
  return (Node l' r')

-- ==========
-- ZADANIE 3. ✅
-- ==========

newtype StateRes s a = StateRes {runStateRes :: s -> s -> (a, s)}
  deriving (Functor)

instance Applicative (StateRes s) where
  pure a = StateRes $ \s _ -> (a, s)
  (<*>) = ap

instance Monad (StateRes s) where
  -- (>>=) :: StateRes s a -> (a -> StateRes s b) -> StateRes s b
  m >>= f = StateRes $ \s t ->
    let (a, s') = runStateRes m s t in runStateRes (f a) s' t

getr :: StateRes s s
getr = StateRes $ \s _ -> (s, s)

getResult :: StateRes s s
getResult = StateRes $ \s t -> (t, s)

putr :: s -> StateRes s ()
putr s = StateRes $ \_ _ -> ((), s)

updater :: (s -> s) -> StateRes s ()
updater f = getr >>= (putr . f)

execStateRes :: StateRes s a -> s -> (a, s)
execStateRes m s = (a, t)
  where
    (a, t) = runStateRes m s t

repmin2 :: Tree -> Tree
repmin2 t = t'
  where
    (t', Min n) = execStateRes (aux2 t) Id

aux2 :: Tree -> StateRes (Min Int) Tree
-- (<*>) :: m (Min Int -> Tree) -> m (Min Int) -> m Tree
aux2 (Leaf n) = do
  updater (toMin n <>)
  Leaf . fromMin <$> getResult
aux2 (Node l r) = do
  l' <- aux2 l
  r' <- aux2 r
  pure (Node l' r')

-- ==========
-- ZADANIE 4. ✅
-- ==========

-- Monad transformers

class MonadTrans (t :: (Type -> Type) -> Type -> Type) where
  lift :: (Monad m) => m a -> t m a

-- MaybeT

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}
  deriving (Functor)

instance (Monad m) => Applicative (MaybeT m) where
  pure = MaybeT . pure . Just
  (<*>) = ap

instance (Monad m) => Monad (MaybeT m) where
  (MaybeT m) >>= f = MaybeT $ do
    ma <- m
    case ma of
      Just a -> runMaybeT $ f a
      Nothing -> pure Nothing

instance MonadTrans MaybeT where
  lift = MaybeT . fmap Just

err :: (Applicative m) => MaybeT m a
err = MaybeT $ pure Nothing

-- ReaderT

newtype ReaderT s m a = ReaderT {runReaderT :: s -> m a}
  deriving (Functor)

instance (Monad m) => Applicative (ReaderT s m) where
  pure = ReaderT . const . pure
  (<*>) = ap

instance (Monad m) => Monad (ReaderT s m) where
  m >>= f = ReaderT $ \ctx -> do
    a <- runReaderT m ctx
    runReaderT (f a) ctx

instance MonadTrans (ReaderT s) where
  lift = ReaderT . const

ask :: (Monad m) => ReaderT s m s
ask = ReaderT pure

-- RPN

type Ident = String

data Cmd = Val Int | Op String | Var Ident

type RPN = [Cmd]

type Env = [(Ident, Int)]

ex1, ex2, ex3 :: RPN
ex1 = [Val 2, Val 3, Op "+", Val 10, Op "*"]
ex2 = [Val 2, Op "+", Val 10, Op "*"]
ex3 = [Val 2, Var "x", Op "+", Var "y", Op "*"]

env1 :: Env
env1 = [("x", 3), ("y", 10)]

op :: String -> Int -> Int -> Int
op "+" = (+)
op "*" = (*)

type Stack = State [Int]

type RPNMonad a = MaybeT (ReaderT Env Stack) a

askEnv :: Ident -> RPNMonad Int
askEnv id = do
  dict <- lift ask
  maybe err pure (lookup id dict)

push :: Int -> RPNMonad ()
push n = (lift . lift) $ update (n :)

pop :: RPNMonad Int
pop = do
  s <- (lift . lift) get
  case s of
    [] -> err
    (x : xs) -> do
      (lift . lift) (put xs)
      pure x

evalCmd :: Cmd -> RPNMonad ()
evalCmd (Val n) = push n
evalCmd (Op s) = do
  x <- pop
  y <- pop
  push (op s x y)
evalCmd (Var id) = do
  n <- askEnv id
  push n

evalRPN :: RPN -> Env -> Maybe Int
evalRPN rpn env = 
  fst 
  $ (`runState` []) 
  $ runReaderT (runMaybeT $ mapM_ evalCmd rpn >> pop) env

-- ==========
-- ZADANIE 5. 🫠
-- ==========

newtype ListT m a = ListT {runListT :: m [a]}
  deriving (Functor)

instance (Monad m) => Applicative (ListT m) where
  pure a = ListT $ pure [a]
  (<*>) = ap

instance (Monad m) => Monad (ListT m) where
  -- (>>=) :: ListT m a -> (a -> ListT m b) -> ListT m b
  -- mapM :: (a -> m [b]) -> [a] -> m [[b]]
  ListT m >>= f = ListT $ do
    xs <- m
    ys <- mapM (runListT . f) xs
    pure (concat ys)

instance MonadTrans ListT where
  -- lift :: m a -> ListT m a
  -- fmap :: (a -> [a]) -> m a -> m [a]
  lift = ListT . fmap pure

{-
  Monadic laws:
  1. return a >>= k = k a
  2. m >>= return = m
  3. m >>= (\x -> k x >>= h) = (m >>= k) >>= h ❗
-}

type Counterexample = ListT [] Int

foobar :: Int -> Counterexample
foobar 0 = ListT [[0, 1]]
foobar 1 = ListT [[0], [1]]

foo :: Counterexample
foo = foobar 0 >>= (\x -> foobar x >>= foobar)

bar :: Counterexample
bar = (foobar 0 >>= foobar) >>= foobar

-- runListT foo /= runListT bar
