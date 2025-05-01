module Lista2 where

-- Zad 1. (Enigma)
import Data.Maybe

data Rotor = Rotor { wiring   :: [Char]
                   , turnover :: [Char] }
  deriving(Show)

data Direction = Encode | Decode 

encodeSingle :: Char -> [Rotor] -> Char 
encodeSingle = foldl f where 
  f acc r = let mapping = zip ['A'..'Z'] (wiring r) in 
    fromMaybe acc (lookup acc mapping)

decodeSingle :: Char -> [Rotor] -> Char 
decodeSingle = foldr f where 
  f r acc = let mapping = zip (wiring r) ['A'..'Z'] in 
    fromMaybe acc (lookup acc mapping)

shiftRotor :: Rotor -> Direction -> Rotor 
shiftRotor (Rotor w t) dir = Rotor (nw dir) t where
  nw Encode = take (length w) $ tail (cycle w)
  nw Decode = last w : init w  

changeConfig :: Direction -> [Rotor] -> [Rotor]
changeConfig dir rs = aux dir rs True where 
  aux _ [] _ = [] 
  aux _ rs False = rs 
  aux Encode (r:rs) True = 
    shiftRotor r Encode : aux Encode rs (head (wiring r) `elem` turnover r)
  aux Decode (r:rs) True = let nr = shiftRotor r Decode in 
    nr : aux Decode rs (head (wiring nr) `elem` turnover nr)

encode :: [Rotor] -> String -> String
encode _ "" = ""
encode rs (x:xs) = encodeSingle x rs : encode (changeConfig Encode rs) xs    

decode :: [Rotor] -> String -> String
decode rs xs = reverse $ aux rs (reverse xs) where 
  aux _ "" = ""
  aux rs (x:xs) = decodeSingle x rs : aux (changeConfig Decode rs) xs 

rotors :: [Rotor]
rotors =
  [ Rotor "EKMFLGDQVZNTOWYHXUSPAIBRCJ" "Q"
  , Rotor "AJDKSIRUXBLHWTMCQGZNPYFVOE" "E"
  , Rotor "BDFHJLCPRTXVZNYEIWGAKMUSQO" "V"
  , Rotor "ESOVPZJAYQUIRHXLNFTGKDCMWB" "J"
  , Rotor "VZBRGITYUPSDNHLXAWMJQOFECK" "Z"
  , Rotor "JPGVOUMFYQBENHZRDKASXLICTW" "ZM"
  , Rotor "NZJHGRCXMYSWBOUFAIVLPEKQDT" "ZM"
  , Rotor "FKQHTLXOCBJSPDZRAMEWNIUYGV" "ZM" ]

rotors2 :: [Rotor]
rotors2 =
  [ Rotor "ABCDEFGHIJKLMNOPQRSTUVWXYZ" "C"
  , Rotor "ABCDEFGHIJKLMNOPQRSTUVWXYZ" "" ]

-- Zad 2 (unfold)`

unfoldStream :: (seed -> (seed, val)) -> seed -> [val]
unfoldStream f s = 
    let (nseed, nval) = f s 
    in nval : unfoldStream f nseed 

-- Zad 3 (Pascal)

pascal :: [[Integer]]
pascal = unfoldStream (\xs -> (1 : aux xs, xs)) [1] where 
  aux [x] = [x] 
  aux (x : y : xs) = (x + y) : aux (y : xs)  

-- Zad 4 (generowanie drzew)

data Tree = Leaf | Node Tree Tree

instance Show Tree where
  show Leaf = "."
  show (Node l r) = "(" ++ show l ++ show r ++ ")"

trees :: [Tree]
trees = concatMap treesN [0..]

treesN :: Integer -> [Tree]
treesN 0 = [Leaf]
treesN n = concatMap (\k -> [Node left right | left <- treesN k, right <- treesN (n-1-k)]) [0..n-1]

-- Zad 5 (lista dwukierunkowa)

data DList a = DCons { val  :: a
                     , prev :: Maybe (DList a)
                     , next :: Maybe (DList a) }

toDList :: [a] -> Maybe (DList a)
toDList xs = aux xs Nothing where 
    aux [] _ = Nothing 
    aux (x:xs) prev = let curr = Just (DCons x prev (aux xs curr)) in curr

bounce :: Int -> DList a -> a
bounce = go prev next where
  go dir anty 0 ds = val ds
  go dir anty n ds = case dir ds of
                       Just ds' -> go dir anty (n-1) ds'
                       Nothing  -> go anty dir n ds
