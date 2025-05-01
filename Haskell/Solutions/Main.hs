module Main where
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import Control.Monad (guard)
import Data.Char (toUpper)
import Data.Map (Map, empty, lookup, insert, insertWith, findWithDefault)

-- ==========
-- Zadanie 1. ✅
-- ==========

class TrieLike t where 
  add :: (Ord c) => [c] -> t c -> t c 
  freq :: (Ord c) => [c] -> t c -> Int

data Trie c = PNode Int ![(c, Trie c)]
  deriving (Show)

instance TrieLike Trie where 
  add = trieInsert 
  freq = trieFreq

trieEmpty :: Trie c
trieEmpty = PNode 0 []

trieInsert :: (Ord c) => [c] -> Trie c -> Trie c
trieInsert [] (PNode n ys) = PNode (n+1) ys 
trieInsert (x:xs) (PNode n ys) = 
  PNode n $ (x, trieInsert xs trie) : filter (\p -> fst p /= x) ys
  where trie = fromMaybe trieEmpty (Prelude.lookup x ys)

trieFreq :: (Ord c) => [c] -> Trie c -> Int
trieFreq [] (PNode n _) = n 
trieFreq (x:xs) (PNode _ ys) = case Prelude.lookup x ys of 
  Nothing -> 0
  Just t -> trieFreq xs t

zad1 :: Bool 
zad1 = trieFreq "ab" it == 2 
  where it = foldr trieInsert trieEmpty ["ab", "abc", "xx", "ab"]

-- ==========
-- Zadanie 2. ✅
-- ==========

loop :: (TrieLike t) => t Char -> IO ()
loop trie = do 
  putStrLn "Enter a single word: "
  input <- getLine 
  guard (input /= [])
  let query = (map toUpper . head . words) input
      result = freq query trie 
  print result
  loop trie

main :: IO ()
main = do 
  xs <- getArgs
  guard (xs /= [])
  content <- readFile (head xs)
  let ys = map (map toUpper) (words content)
  loop (foldr add emptyTrie' ys) -- Do zmiany implementacji wystarczy podmienić emptyTrie' 

-- ==========
-- Zadanie 3. ✅..?
-- ==========

{- 
  Zmiana listy na mape zbija czas lookup'a do logn, i samo to wystarczy
  żeby zauważyć różnicę; eksperymenty z ! nieznacznie pogarszają wynik;
  nie udało mi się wpaść na żadną istotną równość ({-# RULES #-}); UNPACK
  przy takim typie danych też nie ma zbytnio sensu... nie wiem czy to się liczy
  ale to mój najlepszy strzał
-}
data Trie' c = PNode' Int (Map c (Trie' c)) -- "fuzja" zad1 i zad4

emptyTrie' :: Trie' c 
emptyTrie' = PNode' 0 empty 

insertTrie' :: (Ord c) => [c] -> Trie' c -> Trie' c 
insertTrie' [] (PNode' n m) = PNode' (n+1) m
insertTrie' (x:xs) (PNode' n m) = 
  let t = insertTrie' xs (findWithDefault emptyTrie' x m) in
    PNode' n $ insert x t m

freqTrie' :: (Ord c) => [c] -> Trie' c  -> Int
freqTrie' [] (PNode' n _) = n 
freqTrie' (x:xs) (PNode' _ m) =
  case Data.Map.lookup x m of 
    Nothing -> 0
    Just t -> freqTrie' xs t

instance TrieLike Trie' where 
  add = insertTrie' 
  freq = freqTrie'

-- ==========
-- Zadanie 4. ✅..?
-- ==========

newtype TrieMap c = TrieMap 
  { getTrieMap :: Map [c] Int }

instance TrieLike TrieMap where 
  add xs t = TrieMap $ insertWith (+) xs 1 (getTrieMap t)
  freq xs t = findWithDefault 0 xs (getTrieMap t)

{-
[test.sh]
curl https://www.gutenberg.org/cache/epub/4300/pg4300.txt > ulysses.txt
curl https://raw.githubusercontent.com/powerlanguage/word-lists/refs/heads/master/1000-most-common-words.txt > common_words.txt
ghc -package containers Main.hs
time ./Main ulysses.txt < common_words.txt

[./test.sh ran with TrieMap]
real    0m0,820s
user    0m0,759s
sys     0m0,057s

[./test.sh ran with Trie (no-opt)]
real    0m2,257s
user    0m2,104s
sys     0m0,140s

[./test.sh ran with Trie' (opt)]
real    0m1,145s
user    0m1,041s
sys     0m0,096s

wnioski: algorytmy i struktury danych nie są moją mocną stroną
-}

-- ==========
-- Zadanie 5. ✅
-- ==========

collect :: (Ord c) => Trie c -> Int 
collect (PNode n ys) = n + foldr ((+) . collect . snd) 0 ys

check :: (Ord c) => [c] -> Trie c -> Int 
check [] root = collect root
check (x:xs) (PNode _ ys) = case Prelude.lookup x ys of 
  Nothing -> 0
  Just t -> check xs t 

trieFreqSub :: (Ord c) => [c] -> Trie c -> Int 
trieFreqSub [] root = collect root
trieFreqSub s root@(PNode _ ys) =
  let curr = check s root 
      next = foldr ((+) . trieFreqSub s . snd) 0 ys in 
        curr + next

zad5 :: Bool
zad5 = trieFreqSub "ab" it == 4 where 
  it = foldr trieInsert trieEmpty ["abxab", "xab", "xab"]