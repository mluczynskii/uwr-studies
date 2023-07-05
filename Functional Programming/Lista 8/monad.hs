-- Zadanie 9
import Control.Monad

data StreamTrans i o a 
    = Return a 
    | ReadS (Maybe i -> StreamTrans i o a)
    | WriteS o (StreamTrans i o a)

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
