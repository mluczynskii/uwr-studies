{-# LANGUAGE UndecidableInstances #-}

module Lista5 where

import Control.Monad (ap)
import Data.Maybe (isJust, isNothing)
import Data.Char (toUpper)
import Control.Applicative (liftA2)
import Data.List (intercalate)
import Text.Read

-- =========
-- Zadanie 1 ✅ 
-- =========

combineErrors :: (Semigroup e) => Either e a -> Either e b -> Either e (a, b)
combineErrors x y = case (x, y) of 
    (Left e, Left e') -> Left (e <> e') 
    (v, v') -> liftA2 (,) v v'

data Expr = EOp String Expr Expr
          | Val Int
  deriving(Show)

eval :: Expr -> Either [String] Int
eval (Val n) = Right n 
eval (EOp "+" e e') = case combineErrors (eval e) (eval e') of 
  Right (n, n') -> Right (n + n')
  Left xs -> Left xs 
eval (EOp "/" e e') = case combineErrors (eval e) (eval e') of 
  Right (n, 0) -> Left ["Division by zero"]
  Right (n, n') -> Right (n `div` n')
  Left xs -> Left xs 
eval (EOp op e e') = case combineErrors (eval e) (eval e') of
    Left xs -> Left ("Unknown operator" : xs)
    _ -> Left ["Unknown operator"]

expr :: Expr 
expr = EOp "?" (EOp "+" (Val 1) (Val 1)) (EOp "/" (Val 1) (Val 0)) 
  
-- =========
-- Zadanie 2 ✅
-- =========

data Symbol = X | O

newtype Board = Board {state :: [Maybe Symbol]}

-- utility functions
instance Eq Symbol where 
  X == X = True 
  O == O = True
  _ == _ = False 

instance Show Symbol where 
  show X = "X"
  show O = "O"

swap :: Symbol -> Symbol 
swap X = O 
swap O = X

filterIdx :: (Int -> Bool) -> [a] -> [a]
filterIdx f xs = aux f (zip xs [0..]) where 
  aux _ [] = []
  aux f ((x, n) : xs) = if f n then x : aux f xs else aux f xs

getRows :: Board -> [[Maybe Symbol]]
getRows (Board state) = [filterIdx p state | p <- [f 0 2, f 3 5, f 6 8]] where 
  f a b n = (a <= n) && (n <= b)

getCols :: Board -> [[Maybe Symbol]]
getCols (Board state) = [filterIdx p state | p <- [f 0, f 1, f 2]] where 
  f off n = mod (n - off) 3 == 0

getDiag :: Board -> [[Maybe Symbol]]
getDiag (Board state) = [filterIdx p state | p <- [f, f']] where 
  f = flip elem [0, 4, 8]
  f' = flip elem [2, 4, 6]

instance Show Board where
  show board = intercalate "\n" (map (map toChar) (getRows board)) where 
    toChar Nothing = '_'
    toChar (Just X) = 'X'
    toChar (Just O) = 'O' 
          
emptyBoard :: Board
emptyBoard = Board [Nothing | _ <- [0..8]]

wins :: Board -> Maybe Symbol
wins board = let xs = concat [get board | get <- [getRows, getCols, getDiag]]
                 pred sym = all (== Just sym) in 
    if any (pred X) xs then Just X 
    else if any (pred O) xs then Just O 
    else Nothing
    
draw :: Board -> Bool
draw board = all isJust (state board) && isNothing (wins board)

put :: Symbol -> Int -> Board -> Maybe Board
put _ n _ | n < 0 = Nothing -- index out of bounds (< 0)
put sym n (Board state) = case splitAt n state of 
  (_, []) -> Nothing -- index out of bounds (> 8)
  (_, (Just _) : _) -> Nothing -- illegal move 
  (ls, Nothing : rs) -> let state' = ls ++ (Just sym : rs) in 
    Just (Board state')  

play :: Symbol -> Board -> IO ()
play turn board = do 
  print board 
  case (wins board, draw board) of -- check end conditions
    (Just s, _) -> putStrLn ("\n" ++ show s ++ " wins!")
    (_, True) -> putStrLn "\nDraw!"
    (Nothing, False) -> do
      putStr ("\n" ++ show turn ++ "'s move: ")
      move <- getLine 
      case readMaybe move :: Maybe Int of -- check if input is an integer
        (Just idx) -> case put turn idx board of -- check if the move is legal
          Nothing -> do 
            putStrLn "Illegal move, pick a free space in [0..8]!"
            play turn board
          (Just board') -> play (swap turn) board' -- next turn with updated board
        Nothing -> do 
          putStrLn "You were supposed to give an integer..." 
          play turn board

ticTacToe :: IO ()
ticTacToe = play X emptyBoard

-- =========
-- Zadanie 3 ✅
-- =========

data IOTree a = Leaf a
              | Node (IO Bool) (IOTree a) (IOTree a)

runTree :: IOTree a -> IO a
runTree (Leaf val) = pure val 
runTree (Node action l r) = do 
  confirm <- action
  if confirm then runTree l else runTree r 

yesNo :: String -> IO Bool
yesNo s = do 
  putStrLn (s ++ " (y/n):") 
  confirm <- getLine
  if confirm == "y" 
    then pure True 
    else pure False

system :: IOTree String
system =
  Node (yesNo "Czy chcesz wydac pieniadze?")
    (Node (yesNo "Czy chcesz ogladac reklamy?")
      (Leaf "Windows")
      (Leaf "MacOS"))
    (Node (yesNo "Czy umiesz zrobic 'mount'?")
      (Leaf "Arch")
      (Leaf "Ubuntu"))

-- =========
-- Zadanie 4 ✅
-- =========

data Term sig x = Var x
                | Op (sig (Term sig x))
  deriving (Functor)
                
deriving instance (Show x, Show (sig (Term sig x))) => Show (Term sig x) 

instance (Functor sig) => Monad (Term sig) where
  (>>=) :: Term sig x -> (x -> Term sig y) -> Term sig y
  Var n >>= f = f n 
  Op s >>= f = Op (fmap (>>= f) s)

instance (Functor sig) => Applicative (Term sig) where
  pure = Var
  (<*>) = ap

data MyIOSig a = PutStr String a | GetLine (String -> a)
  deriving (Functor)

type MyIO = Term MyIOSig

myPutStr :: String -> MyIO ()
myPutStr s = (Op . PutStr s . pure) ()

myGetLine :: MyIO String
myGetLine = Op (GetLine pure)

echo :: MyIO ()
echo = myGetLine >>= myPutStr 

toRealIO :: MyIO a -> IO a
toRealIO (Var a) = pure a
toRealIO (Op (PutStr s term)) = do 
  putStrLn s 
  toRealIO term 
toRealIO (Op (GetLine f)) = do 
  input <- getLine
  toRealIO (f input)

scream :: MyIO () -- Test to see if it works
scream = do
  myPutStr "I repeat whatever you say, but LOUDER! Try me: "
  input <- myGetLine
  myPutStr (map toUpper input)

-- =========
-- Zadanie 5
-- =========

-- Uwaga: inne nazwy konstruktorow ze wzgledu na clash z Zad 4.

data Request = RPutStr String
             | RGetLine

data Response = RUnit
              | RValue String

type ReReIO = [Response] -> [Request]

echoR :: ReReIO
echoR ~(RValue s : _) = [RGetLine, RPutStr s]

reReToIO :: ReReIO -> IO ()
reReToIO f = undefined

