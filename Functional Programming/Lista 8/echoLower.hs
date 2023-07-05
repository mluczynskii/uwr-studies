-- Zadanie 1
import System.IO
import Data.Char

echoLower :: IO ()
echoLower = do
    check <- System.IO.isEOF
    if check 
        then return ()
        else do 
            c <- getChar
            putChar $ toLower c
            putChar '\n'
            echoLower