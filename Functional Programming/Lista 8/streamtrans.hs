import Data.Char
import System.IO
import Control.Monad
import Control.Exception

-- Zadadnie 2
data StreamTrans i o a 
    = Return a
    | ReadS (Maybe i -> StreamTrans i o a)
    | WriteS o (StreamTrans i o a)

toLower :: StreamTrans Char Char a
toLower = 
    ReadS (\i -> case i of
        Just c  -> WriteS (Data.Char.toLower c) Main.toLower 
        Nothing -> Main.toLower)

runIOStreamTrans :: StreamTrans Char Char a -> IO a
runIOStreamTrans s =
    case s of 
        ReadS f -> do 
            check <- System.IO.isEOF
            if check 
                then runIOStreamTrans (f Nothing) 
                else do
                    c <- getChar 
                    runIOStreamTrans (f $ Just c)
        WriteS x cont -> do
            putChar x
            runIOStreamTrans cont
        Return x -> return x

-- Zadanie 3
listTrans :: StreamTrans i o a -> [i] -> ([o] , a)
listTrans s xs =
    case s of 
        ReadS f -> 
            case xs of 
                x : xs -> listTrans (f $ Just x) xs 
                []     -> listTrans (f Nothing) []
        WriteS y cont -> 
            let (ys, a) = listTrans cont xs in 
                (y : ys, a)
        Return y -> ([], y) 

-- Zadanie 4
auxc :: [a] -> StreamTrans a a b -> b
auxc xs s =
    case s of 
        Return x -> x 
        WriteS x cont -> 
            auxc (xs ++ [x]) cont 
        ReadS f -> 
            case xs of
                [] -> auxc [] (f Nothing)
                x : xs -> auxc xs (f $ Just x)

runCycle :: StreamTrans a a b -> b 
runCycle s = auxc [] s

-- Zadanie 5
(|>|) :: StreamTrans i m a -> StreamTrans m o b -> StreamTrans i o b
(|>|) (Return x) (ReadS f) = Return x |>| f Nothing 
(|>|) (WriteS x cont) (ReadS f) = cont |>| (f $ Just x) 
(|>|) (ReadS f) s = ReadS (\i -> f i |>| s)
(|>|) s (WriteS x cont) = WriteS x (s |>| cont)
(|>|) _ (Return x) = Return x  

-- Zadanie 6
aux :: [o] -> StreamTrans i o a -> StreamTrans i b (a, [o])
aux xs s =
    case s of 
        Return x -> Return (x, xs)
        ReadS f  -> ReadS (\i -> aux xs (f i))
        WriteS x cont -> aux (xs ++ [x]) cont

catchOutput :: StreamTrans i o a -> StreamTrans i b (a, [o])
catchOutput s = aux [] s 

-- Zadanie 9
instance Functor (StreamTrans i o) where
    fmap f m = m >>= return . f

instance Applicative (StreamTrans i o) where 
    pure = Return 
    (<*>) = Control.Monad.ap

instance Monad (StreamTrans i o) where 
    return = pure 
    (>>=) m f =
        case m of 
            Return x -> f x 
            ReadS g -> ReadS (\i -> g i >>= f)
            WriteS x cont -> WriteS x (cont >>= f)

-- Zadanie 7
data BF
    = MoveR -- >
    | MoveL -- <
    | Inc -- +
    | Dec -- -
    | Output -- .
    | Input -- ,
    | While [BF] -- [ ]

parseSimple :: Char -> BF 
parseSimple '<' = MoveR
parseSimple '>' = MoveL
parseSimple '+' = Inc
parseSimple '-' = Dec
parseSimple '.' = Output 
parseSimple ',' = Input 

filter :: StreamTrans Char Char ()
filter = ReadS $ \i -> case i of
    Nothing -> Return ()
    Just c -> if elem c ['<','>','+','-','.',',','[',']'] 
        then WriteS c (Main.filter) 
        else Main.filter 

data Exc 
    = Fail 
    deriving Show
instance Exception Exc 

parseWhile :: StreamTrans Char BF ()
parseWhile = do 
    (_, xs) <- catchOutput $ parser True 
    WriteS (While xs) (parser False)

parser :: Bool -> StreamTrans Char BF ()
parser flag = ReadS $ \i -> case i of 
    Nothing -> if flag 
        then throw Fail -- Unclosed While loop
        else Return ()
    Just c -> if elem c ['<','>','+','-','.',',']
        then WriteS (parseSimple c) (parser flag)
        else if c == ']' 
            then if flag 
                then Return ()
                else throw Fail -- Syntax error
            else parseWhile

brainfuckParser :: StreamTrans Char BF ()
brainfuckParser = Main.filter |>| parser False

