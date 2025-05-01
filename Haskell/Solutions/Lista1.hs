module Lista1 where 

-- Zadanie 1.
factorial :: Integer -> Integer 
factorial n = aux n 1 where 
    aux 0 acc = acc
    aux n acc = aux (n-1) (n*acc) 

-- Zadanie 2.
ack :: Integer -> Integer -> Integer -> Integer 
ack m n 0 = m + n 
ack m 0 1 = 0
ack m 0 2 = 1
ack m 0 _ = m
ack m n p = ack m (ack m (n-1) p) (p-1)

-- Zadanie 3.
removeVowels :: String -> String 
removeVowels = filter (`notElem` "aeoiuyAEOIUY")

-- Zadanie 4.
filterIdx :: (Integer -> Bool) -> [a] -> [a]
filterIdx p xs = aux p xs [0..] [] where 
    aux _ [] _ acc = acc
    aux p (x:xs) (y:ys) acc = if p y then aux p xs ys (x:acc) else aux p xs ys acc 

everyOtherIn :: [a] -> [a]
everyOtherIn = filterIdx even

everyOtherEx :: [a] -> [a]
everyOtherEx = filterIdx odd

merge :: [a] -> [a] -> [a]
merge xs [] = xs 
merge [] ys = ys 
merge (x:xs) (y:ys) = x : y : merge xs ys

-- Zadanie 5
transpose :: [[a]] -> [[a]]
transpose ([]:_) = [] -- zał. że wejście jest poprawną macierzą
transpose x = map head x : transpose (map tail x)

-- Zadanie 6
primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]

primeFactors :: Integer -> [Integer]
primeFactors n = aux n primes where 
    aux 1 _ = []
    aux m (x:xs) = if m `mod` x == 0 then x : aux (m `div` x) (x:xs) else aux m xs
