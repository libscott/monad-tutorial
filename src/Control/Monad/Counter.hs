
module Control.Monad.Counter
  ( Counter(..)
  , unCounter
  ) where

import Control.Applicative

import Debug.Trace

data Counter a = Counter (Int -> a)

unCounter :: Counter a -> Int -> a
unCounter (Counter f) i = f i

instance Show (Counter a) where
    show c = "I am a counter"

instance Functor Counter where
    fmap f (Counter f1) = Counter (f . f1)

instance Applicative Counter where
    pure a = Counter (\_ -> a)
    (Counter f) <*> (Counter a) = Counter (\i -> f i (a i))

instance Monad Counter where
    (Counter f) >>= b =
        Counter (\i -> let a = f i
                        in seq i (unCounter (b a) (traceShowId (i+1))))
    
